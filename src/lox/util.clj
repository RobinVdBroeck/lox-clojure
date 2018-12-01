(ns lox.util)

(defn alpha? [char]
  "Is char an alphabetic [a-zA-Z]"
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

(defn whitespace? [char]
  "Is char whitespace? Newlines are not considered whitespacse"
  (if char
    (if (or (= \space char)
            (= \tab char))
      char)))

(defn newline? [char]
  "Is the char a newline charachter? Only \n is consider a newline."
  (if char
    (if (= \n char)
      char)))

(defn digit? [char]
  "Is a char a digit"
  (if char
    (let [zero (int \0)
          nine (int \9)
          ichar (int char)]
      (if (and (>= ichar zero)
               (<= ichar nine))
        char))))

