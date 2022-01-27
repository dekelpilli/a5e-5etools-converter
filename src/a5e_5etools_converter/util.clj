(ns a5e-5etools-converter.util
  (:require [clojure.edn :as edn])
  (:import (java.util.concurrent TimeUnit)
           (java.util Date)))

(def source-id "LevelUpAdventurersGuideA5E")

(defn ->num [s]
  (try
    (let [n (edn/read-string s)]
      (when (number? n) n))
    (catch Exception _)))

(defn idx-in-bounds? [idx coll]
  (<= idx (dec (count coll))))

(defn source-meta []
  (let [now (->> (Date.) (inst-ms) (.toSeconds TimeUnit/MILLISECONDS))]
    {:sources          [{:json         source-id
                         :abbreviation "A5E"
                         :full         "Level Up: Adventurers Guide (A5E)"
                         :url          "https://www.levelup5e.com/"
                         :authors      ["Level Up"]
                         :convertedBy  ["TODO DMs"]
                         :version      "0.0.1"}]
     :dateAdded        now
     :dateLastModified now}))
