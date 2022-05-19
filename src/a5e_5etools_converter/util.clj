(ns a5e-5etools-converter.util
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [jsonista.core :as j])
  (:import (java.util.concurrent TimeUnit)
           (java.util Date)
           (java.io File)))

(def a5e-source-id "LevelUpAdventurersGuideA5E")
(def phb-source-id "PHB")

(defn starts-with-pred [& prefixes]
  (fn [s]
    (reduce #(when (str/starts-with? s %2)
               (reduced true)) false prefixes)))

(defn load-json [^String file-name]
  (try
    (j/read-value (File. file-name))
    (catch Exception _ nil)))

(defn ->num [s]
  (try
    (let [n (edn/read-string s)]
      (when (number? n) n))
    (catch Exception _ s)))

(defn- idx-in-bounds? [idx coll]
  (<= idx (dec (count coll))))

(defn extract-lines [^String file-name line-remove-pred new-entity-pred]
  (let [lines (->> (File. file-name)
                   (slurp)
                   (str/split-lines)
                   (remove line-remove-pred)
                   (vec))]
    (loop [entities []
           lines lines]
      (let [idx (loop [new-entity-lines 0
                       idx 0]
                  (let [line (nth lines idx)]
                    (if (new-entity-pred line)
                      (if (zero? new-entity-lines)
                        (recur (inc new-entity-lines) (inc idx))
                        (dec idx))
                      (let [new-idx (inc idx)]
                        (if (idx-in-bounds? new-idx lines)
                          (recur new-entity-lines new-idx)
                          new-idx)))))
            [entity lines] (split-at idx lines)]
        (if (or (empty? lines) (empty? entity))
          (conj entities entity)
          (if (zero? idx)
            (conj entities entity)
            (recur (conj entities entity) lines)))))))

(defn lines->entries [lines]
  (cond-> (loop [entry-lines []
                 entries []
                 [current & remaining] lines]
            (if current
              (let [current (-> current
                                (str/trim)
                                (str/replace
                                  #"(([1-9]\d*)?[Dd][1-9]\d*?( ?[+-] ?[0-9]\d*)?)"
                                  "{@dice $1}")
                                (str/replace #"(?i)(blinded|charmed|deafened|exhaustion|frightened|grappled|incapacitated|invisible|paralyzed|petrified|poisoned|prone|rattled|restrained|slowed|stunned|unconscious)"
                                             (comp #(str "{@condition " % "}")
                                                   #(cond-> (if-let [custom-condition
                                                                     (get
                                                                       {"paralyzed"     "staggered"
                                                                        "stunned"       "debilitated"
                                                                        "incapacitated" "dazed"
                                                                        "confused"      "confused"
                                                                        "rattled"       "rattled"
                                                                        "slowed"        "slowed"} %)]
                                                              (str custom-condition "|" a5e-source-id)
                                                              %)
                                                            (re-matches #"^[A-Z].*" %) (str/capitalize))
                                                   first)))]
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
          (str/includes? lines "TABLE") (conj "<<<ADD TABLE MANUALLY>>>")
          (str/includes? lines "STR DEX") (conj "<<<ADD STAT BLOCK MANUALLY>>>")
          (str/includes? lines "•") (conj "<<<FIX LIST MANUALLY>>>")))

(defn source-meta []
  (let [now (->> (Date.) (inst-ms) (.toSeconds TimeUnit/MILLISECONDS))]
    {:sources          [{:json         a5e-source-id
                         :abbreviation "A5E"
                         :full         "Level Up: Adventurers Guide (A5E)"
                         :url          "https://www.levelup5e.com/"
                         :authors      ["Level Up"]
                         :convertedBy  ["TODO DMs"]
                         :version      "0.0.1"}]
     :dateAdded        now
     :dateLastModified now}))

(defn ->file [^String file-name coll]
  (.writeValue j/keyword-keys-object-mapper
               (File. file-name)
               coll))
