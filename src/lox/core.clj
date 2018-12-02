(ns lox.core
  (:gen-class)
  (:require [lox.scanner :refer [program->tokens]]
            [clojure.pprint :refer [pprint]])
  (:import (java.io FileNotFoundException)))

(defn run-program
  [program]
  (-> program
      program->tokens
      pprint))

(def quit-strings #{"quit" ":quit" "(quit)"
                    "exit" ":exit" "(exit)"})

(defn run-repl
  []
  (loop []
    (print "> ")
    (flush)
    (let [line (read-line)]
      (if-not (contains? quit-strings line)
        (do
          (run-program line)
          (recur))
        (println "Thanks for using Lex, have a great day")))))

(defn run-file
  [filename]
  (try
    (let [content (slurp filename)]
      (run-program content))
    (catch FileNotFoundException _
      (println "Could not read file" filename))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (condp = (count args)
    0 (run-repl)
    1 (run-file (first args))
    (println "Usage: jlox [script]")))
