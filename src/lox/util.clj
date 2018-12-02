(ns lox.util)

(defn alpha?
  "Is char an alphabetic [a-zA-Z]"
  [char]
  (if char
    (let [ichar (int char)
          a (int \a)
          z (int \z)
          A (int \A)
          Z (int \Z)]
      (if (or (and (>= ichar a)
                   (<= ichar z))
              (and (>= ichar A)
                   (<= ichar Z)))
        char))))

(defn whitespace?
  "Is char a whitespace? Newlines are not considered whitespace"
  [char]
  (if char
    (if (or (= \space char)
            (= \tab char))
      char)))

(defn newline?
  "Is char a newline? Only \n is consider a newline."
  [char]
  (if char
    (if (= \n char)
      char)))

(defn digit?
  "Is a char a digit [0-9]"
  [char]
  (if char
    (let [zero (int \0)
          nine (int \9)
          ichar (int char)]
      (if (and (>= ichar zero)
               (<= ichar nine))
        char))))

