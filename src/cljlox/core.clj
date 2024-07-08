(ns cljlox.core
  (:require [cljlox.scanner :as scanner])
  (:gen-class))

(defn run [source]
  (let [tokens (scanner/scan source)]
    (doseq [token tokens]
      (println token)
      (flush))))

(defn run-file [fname]
  (run (slurp fname)))

(defn run-prompt []
  (print "> ")
  (flush)
  (loop [line (read-line)]
    (when (not= line nil)
      (run line)
      (print "> ")
      (flush)
      (recur (read-line)))))

(defn -main
  ([] (run-prompt))
  ([file] (run-file file))
  ([_ & _]
   (println "Usage: cljlox [script]")
   (System/exit 64)))
