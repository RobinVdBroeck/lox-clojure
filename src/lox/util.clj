(ns lox.util
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

(s/fdef alpha?
        :args (s/cat :char (s/nilable char?))
        :ret (s/nilable char?))
(defn alpha?
  "Is char an alphabetic [a-zA-Z]"
  [char]
  (if char
    (let [ichar (int char)
          a (int \a)
          z (int \z)
          A (int \A)
          Z (int \Z)]
      (if (or (<= a ichar z)
              (<= A ichar Z))
        char))))

(s/fdef whitespace?
        :args (s/cat :char (s/nillable char?))
        :ret (s/nillable char?))
(defn whitespace?
  "Is char a whitespace? Newlines are not considered whitespace"
  [char]
  (if char
    (if (or (= \space char)
            (= \tab char))
      char)))

(s/fdef newline?
        :args (s/cat :char (s/nillable char?))
        :ret (s/nillable char?))
(defn newline?
  "Is char a newline? Only \n is consider a newline."
  [char]
  (if char
    (if (= \n char)
      char)))

(s/fdef digit?
        :args (s/cat :char (s/nillable char?))
        :ret (s/nillable char?))
(defn digit?
  "Is a char a digit [0-9]"
  [char]
  (if char
    (let [[zero, ichar, nine] (map [\0 char 9] int)]
      (if (<= zero ichar nine)
        char))))

(st/instrument)
