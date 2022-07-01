(ns fivee-tools-converter.toh-spells
  (:require [fivee-tools-converter.util :as u]
            [clojure.string :as str]))

(def spell-level-pred
  #(or
     (re-matches #"(?i)(abjuration|conjuration|divination|enchantment|evocation|illusion|necromancy|psionic|transmutation) cantrip" %)
     (re-matches #"(?i)(1st-level|2nd-level|3rd-level|4th-level|5th-level|6th-level|7th-level|8th-level|9th-level) (abjuration|conjuration|divination|enchantment|evocation|illusion|necromancy|psionic|transmutation)" %)))

(def ->level {"Cantrip"   0
              "1st-level" 1
              "2nd-level" 2
              "3rd-level" 3
              "4th-level" 4
              "5th-level" 5
              "6th-level" 6
              "7th-level" 7
              "8th-level" 8
              "9th-level" 9})

(def special-range-types #{"plane" "sight" "self" "touch" "unlimited" "special"})

(def ->school {"abjuration"    "A"
               "conjuration"   "C"
               "divination"    "D"
               "enchantment"   "E"
               "evocation"     "V"
               "illusion"      "I"
               "necromancy"    "N"
               "psionic"       "P"
               "transmutation" "T"})

(defn ->unit [unit]
  (let [unit (str/replace unit #"\s|,|\(|\)|;" "")]
    (get {"actions"   "action"
          "reactions" "reaction"
          "rounds"    "round"
          "minutes"   "minute"
          "hours"     "hour"
          "days"      "day"
          "weeks"     "week"
          "months"    "month"
          "years"     "year"} unit unit)))

(defn ->sanitised-list [s]
  (if s
    (->> (str/split s #",")
         (map #(str/escape % {\space   ""
                              \newline ""
                              \,       ""
                              \)       ""})))
    []))

(defn extract-spell-lines [^String file-name]
  (u/extract-lines file-name
                   #(re-matches #"^(Chapter 6: Magic and Spells|[1-9][0-9]*)$" %)
                   spell-level-pred))

(def raw-content->entries (comp u/lines->entries str/split-lines))

(defn range-sections->range [range-sections]
  (if-let [special-type (special-range-types (first range-sections))]
    (if (second range-sections)
      (recur (rest range-sections))
      {:type special-type})
    {:amount (u/->num (first range-sections))
     :type   (let [raw-type (second range-sections)]
               (get {"foot" "feet" "mile" "miles"} raw-type raw-type))}))

(defn merge-until-next-section [unparsed-lines section-end-pred]
  (let [first-line (first unparsed-lines)
        unparsed-lines (rest unparsed-lines)
        next-idx (reduce #(if (section-end-pred %2)
                            (reduced %1)
                            (inc %1))
                         0 unparsed-lines)]
    {:lines   (nthrest unparsed-lines next-idx)
     :content (str/join \newline (cons first-line (take next-idx unparsed-lines)))}))

(defn extract-pseudo-section-entries [content section-name]
  {:type "entries" :name section-name :entries [(-> content
                                                    (subs (+ 2 (count section-name)))
                                                    (str/escape {\newline \space}))]})

(defn extract-duration [duration-text]
  (let [duration-text-lower (str/lower-case duration-text)] ;inconsistent casing in pdf
    (cond
      (str/starts-with? duration-text-lower "until")
      {:type "permanent"
       :ends (cond-> []
                     (str/includes? duration-text-lower "dispelled") (conj "dispel")
                     (str/includes? duration-text-lower "trigger") (conj "trigger"))}
      (= duration-text-lower "instantaneous") {:type "instant"}
      (#{"varies" "special"} duration-text-lower) {:type "special"}
      :else (let [concentration? (str/starts-with? duration-text "Concentration ")
                  duration-text-no-conc (cond-> duration-text
                                                concentration? (-> (subs (count "Concentration: "))
                                                                   (str/escape {\( ""
                                                                                \) ""})))
                  up-to? (str/starts-with? (str/lower-case duration-text-no-conc)
                                           "up to")
                  [amount raw-unit] (-> duration-text-no-conc
                                        (cond-> up-to? (subs (count "Up to ")))
                                        (str/trim)
                                        (str/split #" "))
                  special? (= amount "special")]
              (cond-> {:type (cond
                               special? "special"
                               :else "timed")}
                      (not special?) (assoc :duration
                                            {:type   (->unit raw-unit)
                                             :amount (or (parse-long amount) duration-text-no-conc)
                                             :upTo   up-to?})
                      concentration? (assoc :concentration true))))))

(defn extract-spell-sections [_ spell-lines]
  (let [spell-name (first spell-lines)
        spell (loop [spell {:name    spell-name
                            :page    497 ;spell page start, don't care about specifics
                            :source  u/toh-source-id
                            :entries []}
                     section :level
                     unparsed-lines (rest spell-lines)]
                (case section
                  :level (let [
                               _ (println spell-name)
                               {:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                                 (u/starts-with-pred "Casting Time: "))
                               [_ level-suffix :as level-data] (str/split content #" " 2)
                               [level level-suffix] (cond-> level-data
                                                            (str/starts-with? level-suffix "cantrip") (reverse))
                               [_ school _ types] (re-matches #"^(?i)(abjuration|conjuration|divination|enchantment|evocation|illusion|necromancy|psionic|transmutation)( \()?(.)*$"
                                                              level-suffix)
                               types (->sanitised-list types)]
                           (recur
                             (assoc spell :level (->level level)
                                          :subschools types
                                          :school (-> school (str/lower-case) (->school)))
                             :casting-time
                             lines))
                  :casting-time (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                                        (u/starts-with-pred
                                                                                          "Range: "
                                                                                          "Target: "
                                                                                          "Area: "
                                                                                          "Components: "))
                                      [number raw-unit other] (-> (subs content (count "Casting Time: "))
                                                                  (str/split #"\s" 3))
                                      normalised-unit (->unit raw-unit)
                                      amount (cond-> (u/->num number)
                                                     (= "week" normalised-unit) (* 168))
                                      unit (get {"week" "hour"} normalised-unit normalised-unit)]
                                  (recur
                                    (-> spell
                                        (assoc :time [{:number amount
                                                       :unit   unit}])
                                        (cond->
                                          (and other
                                               (str/includes? (str/lower-case other) "ritual")) (assoc-in [:meta :ritual] true)))
                                    :range
                                    lines))
                  :range (let [has-range? (str/starts-with? (first unparsed-lines) "Range: ")
                               [distance lines] (if has-range?
                                                  (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                                                          (u/starts-with-pred
                                                                                                            "Target: "
                                                                                                            "Area: "
                                                                                                            "Components: "))
                                                        range-sections (-> content
                                                                           (subs (count "Range: "))
                                                                           (str/lower-case)
                                                                           (str/replace #"short|medium|long|\(|\)" "")
                                                                           (str/trim)
                                                                           (str/split #"\-| "))]
                                                    [(range-sections->range range-sections) lines])
                                                  [{:type "special"} unparsed-lines])]
                           (recur
                             (-> spell
                                 (assoc-in [:range :type] "point") ;inaccurate, don't care
                                 (assoc-in [:range :distance] distance))
                             :components
                             lines))
                  :components (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                                      (u/starts-with-pred "Duration: "))
                                    raw-components (-> content
                                                       (subs (count "Components: "))
                                                       (str/escape {\newline \space})
                                                       (str/split #"," 3))
                                    components (->> raw-components
                                                    (map (comp #(str/split % #" " 2) str/trim))
                                                    (into {} (map (fn [[component text]] [(str/lower-case component)
                                                                                          (if text
                                                                                            {:text (-> text
                                                                                                       (str/escape {\( ""
                                                                                                                    \) ""})
                                                                                                       (str/trim))}
                                                                                            true)]))))]
                                (recur
                                  (assoc spell :components components)
                                  :duration
                                  lines))
                  :duration (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                                    #(re-matches #"^[A-Z].*" %))
                                  duration-text (-> content
                                                    (subs (count "Duration: "))
                                                    (str/escape {\newline \space})
                                                    (str/trim))
                                  duration-texts (if (str/includes? duration-text "Until dispelled")
                                                   [duration-text]
                                                   (str/split duration-text #" or "))]
                              (recur (assoc spell :duration (map extract-duration duration-texts))
                                     :entries
                                     lines))
                  :entries (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                                   (u/starts-with-pred "Cast at Higher Levels. "
                                                                                                       "Rare: "))]
                             (recur
                               (update spell :entries #(into % (raw-content->entries content)))
                               :higher-levels
                               lines))
                  :higher-levels (if-let [higher-levels-prefix (-> (and
                                                                     (seq unparsed-lines)
                                                                     (re-matches #"^(At Higher Levels.\s?).*$"
                                                                                 (first unparsed-lines)))
                                                                   (second))]
                                   (let [{:keys [content lines]} (merge-until-next-section unparsed-lines (u/starts-with-pred "Rare: "))
                                         entries (-> content
                                                     (subs higher-levels-prefix)
                                                     (raw-content->entries))]
                                     (recur
                                       (assoc spell :entriesHigherLevel
                                                    [{:type    "entries"
                                                      :name    "Cast at Higher Levels"
                                                      :entries entries}])
                                       :rare
                                       lines))
                                   (recur spell :rare unparsed-lines))
                  :rare (if (seq unparsed-lines)
                          (let [{:keys [content lines]} (merge-until-next-section unparsed-lines (u/starts-with-pred "Rare: "))
                                entries (-> content
                                            (subs (count "Rare: "))
                                            (raw-content->entries))]
                            (recur
                              (update spell :entries #(conj % {:type    "entries" :name "Rare"
                                                               :entries entries}))
                              :rare lines))
                          spell)))]
    spell
    #_(cond-> spell
              entries (assoc :entries entries)
              entriesHigherLevel (assoc :entriesHigherLevel entriesHigherLevel))))

(defn convert-spells []
  (let [manual-data (comment (u/load-json "data/kobold/spells/manually-adjusted-spells.json"))
        spell-lines (extract-spell-lines "data/kobold/spells/spells-touched-up.txt")]
    (map #(try
            (extract-spell-sections manual-data %)
            (catch Exception e
              (throw (ex-info (ex-message e) {:original %} e))))
         spell-lines)))

(defn write-spells! []
  (u/->file "data/5et/generated/kobold/spells.json" {:_meta (u/toh-source-meta)
                                                     :spell (convert-spells)}))
