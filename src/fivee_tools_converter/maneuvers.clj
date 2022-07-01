(ns fivee-tools-converter.maneuvers
  (:require [fivee-tools-converter.util :as u]
            [clojure.string :as str]))

(defn extract-maneuver-lines [^String file-name]
  (u/extract-lines file-name
                   #(re-matches #"^(Chapter 9: Combat Maneuvers|Adventurer’s Guide|[1-9][0-9]*)$" %)
                   (u/starts-with-pred "1st degree ", "2nd degree ", "3rd degree ", "4th degree " "5th degree ")))

(defn extract-maneuver-sections [lines]
  (let [[maneuver-name points] (str/split (first lines) #" \(")
        [_ level-str school-and-action] (re-matches
                                          #"^(1st degree |2nd degree |3rd degree |4th degree |5th degree )(.*)$"
                                          (second lines))
        [subschool action-str] (->> (str/split school-and-action #" ")
                                    (split-with (complement #{"bonus" "action" "reaction"}))
                                    (map #(str/join " " %)))
        action-unit (-> action-str
                        (str/replace #"\(|\)" "")
                        (str/split #"\s" 2)
                        (first))
        stance? (str/ends-with? action-str "(stance)")
        entries (->> (for [line (nthrest lines 2) :while (not (re-matches #".*Maneuvers$" line))]
                       line)
                     (u/lines->entries)
                     (into [{:type    "entries"
                             :name    "Points"
                             :entries [(-> points
                                           (subs 0 (dec (count points))))]}]))]
    {:name       maneuver-name
     :page       458
     :source     u/a5e-source-id
     :school     "P"
     :time       [{:number 1
                   :unit   action-unit}]
     :duration   [(if stance?
                    {:type "permanent"
                     :ends ["stance"]}
                    {:type "instant"})]
     :subschools ["Maneuver" (str/trim subschool)]
     :level      (-> level-str (subs 0 1) (u/->num))
     :range      {:type     "point"
                  :distance {:type "special"}}
     :entries    entries}))

(defn convert-maneuvers []
  (let [maneuver-lines (extract-maneuver-lines "data/a5e/maneuvers/maneuvers.txt")
        manual-maneuvers (u/load-json "data/a5e/spells/manually-adjusted-maneuvers.json")]
    (->> maneuver-lines
         (filter #(re-matches #".*\([1-9] point(s)?\)$" (first %)))
         (map extract-maneuver-sections)
         (into manual-maneuvers))))

(defn write-maneuvers! []
  (u/->file "data/5et/generated/a5e/maneuvers.json" {:_meta (u/a5e-source-meta)
                                                     :spell (convert-maneuvers)}))
