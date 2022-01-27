(ns a5e-5etools-converter.spells
  (:require [a5e-5etools-converter.util :as u]
            [jsonista.core :as j]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import (java.io File)
           (com.fasterxml.jackson.databind ObjectMapper)))

(defn spell-level? [s] ;TODO use regex or generic multi-starts-with? fn
  (or (str/starts-with? s "Cantrip (")
      (str/starts-with? s "1st-level (")
      (str/starts-with? s "2nd-level (")
      (str/starts-with? s "3rd-level (")
      (str/starts-with? s "4th-level (")
      (str/starts-with? s "5th-level (")
      (str/starts-with? s "6th-level (")
      (str/starts-with? s "7th-level (")
      (str/starts-with? s "8th-level (")
      (str/starts-with? s "9th-level (")))

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
  (let [unit (str/replace unit #"\s|,|\(|\)" "")]
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
  (->> (str/split s #",")
       (map #(str/escape % {\space   ""
                            \newline ""
                            \,       ""
                            \)       ""}))))

(defn extract-spell-lines [^String file-name]
  (let [lines (->> (File. file-name)
                   (slurp)
                   (str/split-lines)
                   (remove #(re-matches #"^(Chapter 10: Spellcasting|Adventurer’s Guide|[1-9][0-9]*)$" %))
                   (vec))]
    (loop [spells []
           lines lines]
      (let [idx (loop [spell-level-lines 0
                       idx 0]
                  (let [line (nth lines idx)]
                    (if (spell-level? line)
                      (if (zero? spell-level-lines)
                        (recur (inc spell-level-lines) (inc idx))
                        (dec idx))
                      (let [new-idx (inc idx)]
                        (if (u/idx-in-bounds? new-idx lines)
                          (recur spell-level-lines new-idx)
                          new-idx)))))
            [spell lines] (split-at idx lines)]
        (if (or (empty? lines) (empty? spell))
          (conj spells spell)
          (if (zero? idx)
            (conj spells spell)
            (recur (conj spells spell) lines)))))))

(defn raw-content->entries [content]
  (cond-> (loop [entry-lines []
                 entries []
                 [current & remaining] (str/split-lines content)]
            (if current
              (let [current (-> current
                                (str/trim)
                                (str/replace
                                  #"(([1-9]\d*)?[Dd][1-9]\d*)" ;TODO include d + const? e.g. 1d4 + 1 in Time Stop
                                  "{@dice $1}")
                                ;TODO remove stunned/paralyzed/incapacitated in favour of homebrew conditions
                                (str/replace #"(?i)(blinded|charmed|deafened|exhaustion|frightened|grappled|incapacitated|invisible|paralyzed|petrified|poisoned|prone|restrained|stunned|unconscious)"
                                             "{@condition $1}"))]
                (if (re-matches #"[^.]+[\.:]" current)
                  (recur []
                         (conj entries (conj entry-lines current))
                         remaining)
                  (recur (conj entry-lines current)
                         entries
                         remaining)))
              (->> (cond-> entries
                           (seq entry-lines) (conj entry-lines))
                   (mapv #(str/join \space %)))))
          (str/includes? content "TABLE") (conj "<<<ADD TABLE MANUALLY>>>")
          (str/includes? content "•") (conj "<<<FIX LIST MANUALLY>>>")))

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

;https://www.jsonschemavalidator.net/
(defn extract-spell-sections [spell-lines]
  (loop [spell {:name    (first spell-lines)
                :page    497 ;spell page start, don't care about specifics
                :source  "LevelUpAdventurersGuideA5E"
                :entries []}
         section :level
         unparsed-lines (rest spell-lines)]
    (case section
      :level (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                     #(str/starts-with? % "Classes: "))
                   [level level-suffix] (str/split content #" \(" 2)
                   [school types] (str/split level-suffix #";" 2)
                   types (->sanitised-list types)]
               (recur
                 (assoc spell :level (->level level)
                              :subschools types
                              :school (->school school))
                 :classes
                 lines))
      :classes (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                       #(str/starts-with? % "Casting Time: "))
                     classes (-> content
                                 (subs (count "Classes: "))
                                 (->sanitised-list))]
                 (recur (assoc spell :classes {:fromClassList (map (fn [class] {:name   (str/capitalize class)
                                                                                :source "LevelUpAdventurersGuideA5E"}) classes)})
                        :casting-time
                        lines))
      :casting-time (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                            #(or (str/starts-with? % "Range: ")
                                                                                 (str/starts-with? % "Target: ")
                                                                                 (str/starts-with? % "Area: ")
                                                                                 (str/starts-with? % "Components: ")))
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
                                                                                              #(or (str/starts-with? % "Target: ")
                                                                                                   (str/starts-with? % "Area: ")
                                                                                                   (str/starts-with? % "Components: ")))
                                            range-sections (-> content
                                                               (subs (count "Range: "))
                                                               (str/lower-case)
                                                               (str/replace #"short|medium|long|\(|\)" "")
                                                               (str/trim)
                                                               (str/split #"\-| "))]
                                        [(if-let [special-type (special-range-types (first range-sections))]
                                           {:type special-type
                                            ;:amount ;TODO check for amount
                                            #_["Clairvoyance" "Detect Evil and Good" "Detect Magic" "Detect Poison and Disease" "Detect Thoughts" "Dimension Door" "Earthquake" "Grapevine" "Hallucinatory Terrain" "Ice Storm" "Insect Plague" "Locate Animals or Plants" "Locate Creature" "Locate Object" "Magnificent Mansion" "Meteor Swarm" "Mirage Arcane" "Project Image" "Storm of Vengeance" "Wormway"]
                                            }
                                           {:amount (u/->num (first range-sections))
                                            :type   (let [raw-type (second range-sections)]
                                                      (get {"foot" "feet" "mile" "miles"} raw-type raw-type))})
                                         lines])
                                      [{:type "special"} unparsed-lines])]
               (recur
                 (-> spell
                     (assoc-in [:range :type] "point") ;inaccurate, don't care
                     (assoc-in [:range :distance] distance))
                 :target
                 lines))
      :target (if (str/starts-with? (first unparsed-lines) "Target: ")
                (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                        #(or (str/starts-with? % "Area: ")
                                                                             (str/starts-with? % "Components: ")))
                      target (extract-pseudo-section-entries content "Target")]
                  (recur
                    (update spell :entries #(conj % target))
                    :area lines))
                (recur spell :area unparsed-lines))
      :area (if (str/starts-with? (first unparsed-lines) "Area: ")
              (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                      #(str/starts-with? % "Components: "))
                    area (extract-pseudo-section-entries content "Area")
                    area-words (-> area
                                   (:entries)
                                   (first)
                                   (str/lower-case)
                                   (str/replace #"\(|\)|\," "")
                                   (str/trim)
                                   (str/split #"\-| ")
                                   (set))
                    matched-words (set/intersection #{"cube" "hemisphere" "line" "cone" "square"
                                                      "circle" "sphere" "wall" "cylinder"}
                                                    area-words)
                    area-tags (map {"cube"       "C"
                                    "hemisphere" "H"
                                    "line"       "L"
                                    "cone"       "N"
                                    "square"     "Q"
                                    "circle"     "R"
                                    "sphere"     "S"
                                    "wall"       "W"
                                    "cylinder"   "Y"} matched-words)]
                (recur
                  (-> spell
                      (update :entries #(conj % area))
                      (assoc :areaTags area-tags))
                  :components lines))
              (recur spell :components unparsed-lines))
      :components (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                          #(str/starts-with? % "Duration: "))
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
                                                                        #(or (str/starts-with? % "Saving Throw: ")
                                                                             (re-matches #"^[A-Z].*" %)))
                      duration-text (-> content
                                        (subs (count "Duration: "))
                                        (str/escape {\newline \space})
                                        (str/trim))
                      duration-text-lower (str/lower-case duration-text) ;inconsistent casing in pdf
                      duration (cond
                                 (str/starts-with? duration-text-lower "until")
                                 {:type "permanent"
                                  :ends (cond-> []
                                                (str/includes? duration-text-lower "dispelled") (conj "dispel")
                                                (str/includes? duration-text-lower "trigger") (conj "trigger"))}
                                 (= duration-text-lower "instantaneous") {:type "instant"}
                                 (#{"varies" "special"} duration-text-lower) {:type "special"}
                                 :else (let [concentration? (str/starts-with? duration-text "Concentration ")
                                             [amount raw-unit] (-> duration-text
                                                                   (cond->
                                                                     concentration? (-> (subs (count "Concentration: "))
                                                                                        (str/escape {\( ""
                                                                                                     \) ""})))
                                                                   (str/trim)
                                                                   (str/split #" "))
                                             special? (= amount "special")]
                                         (cond-> {:type (if special? "special" "timed")}
                                                 (not special?) (assoc :duration {:type   (->unit raw-unit)
                                                                                  :amount (u/->num amount)})
                                                 concentration? (assoc :concentration true))))]
                  (recur (assoc spell :duration [duration])
                         :saving-throw
                         lines))
      :saving-throw (if (str/starts-with? (first unparsed-lines) "Saving Throw: ")
                      (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                              #(re-matches #"^[A-Z].*" %))
                            saving-throw (extract-pseudo-section-entries content "Saving Throw")
                            types (->> (str/split content #"\s")
                                       (map #{"strength" "dexterity" "constitution" "intelligence" "wisdom" "charisma"})
                                       (filter identity)
                                       (distinct))]
                        (recur
                          (-> spell
                              (update :entries #(conj % saving-throw))
                              (assoc :savingThrow types))
                          :entries lines))
                      (recur spell :entries unparsed-lines))
      :entries (let [{:keys [content lines]} (merge-until-next-section unparsed-lines
                                                                       #(or (str/starts-with? % "Cast at Higher Levels. ")
                                                                            (str/starts-with? % "Rare: ")))]
                 (recur
                   (update spell :entries #(into % (raw-content->entries content)))
                   :higher-levels
                   lines))
      :higher-levels (if (and
                           (seq unparsed-lines)
                           (str/starts-with? (first unparsed-lines)
                                             "Cast at Higher Levels. "))
                       (let [{:keys [content lines]} (merge-until-next-section unparsed-lines #(str/starts-with? % "Rare: "))
                             entries (-> content
                                         (subs (count "Cast at Higher Levels. "))
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
              (let [{:keys [content lines]} (merge-until-next-section unparsed-lines #(str/starts-with? % "Rare: "))
                    entries (-> content
                                (subs (count "Rare: "))
                                (raw-content->entries))]
                (recur
                  (update spell :entries #(conj % {:type    "entries" :name "Rare"
                                                   :entries entries}))
                  :rare lines))
              spell))))

(defn convert-spells []
  (let [spell-lines (extract-spell-lines "data/a5e/spells/a5e-spells-touched-up.txt")]
    (map #(try
            (extract-spell-sections %)
            (catch Exception e
              (throw (ex-info (ex-message e) {:original %} e))))
         spell-lines)))

(defn write-spells []
  (.writeValue ^ObjectMapper j/keyword-keys-object-mapper
               (File. "data/5et/generated/spells.json")
               {:_meta (u/source-meta)
                :spell (convert-spells)}))
