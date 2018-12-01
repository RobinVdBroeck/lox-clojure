(ns lox.scanner
  (:gen-class)
  (:require [lox.util :refer :all]))

(def builtin-keyword-map
  {"and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "for"    :for
   "fun"    :fun
   "if"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while})

(defn increase-current
  ([state] (increase-current state 1))
  ([state amount] (update state :current #(+ % amount))))

(defn get-current-char [{:keys [current program]}]
  (try
    (.charAt ^String program current)
    (catch StringIndexOutOfBoundsException _
      nil)))


(defn at-end? [{:keys [current program]}]
  (>= current (count program)))

(defn get-next-char [state]
  "Returns the next char if there is one"
  (let [next-state (increase-current state)]
    (if-not (at-end? next-state)
      (get-current-char next-state))))


(defn define-single-char-token [char token-type]
  "Static single char token"
  (fn [state]
    {:type     token-type
     :lexeme   (str char)
     :size     1
     :position (:current state)}))


(defn define-double-char-token [first-char second-char no-match-type match-type]
  "Token that may or may not contain 2 chars (example: < | <=)
   If the second char matches, returns the second token"
  (fn [state]
    (if (not (= second-char (get-next-char state)))
      {:type     no-match-type
       :lexeme   (str first-char)
       :size     1
       :position (:current state)}
      {:type     match-type
       :lexeme   (str first-char second-char)
       :size     2
       :position (:current state)})))

(defn single-char-matcher [char token-type]
  [char (define-single-char-token char token-type)])

(defn double-char-matcher [first-char second-char no-match-type match-type]
  [first-char (define-double-char-token first-char second-char no-match-type match-type)])

; Requires types of [:char :factory] to insert into the map
(def char->token-factory
  (into {} [(single-char-matcher \( :left-paren)
            (single-char-matcher \) :right-paren)
            (single-char-matcher \{ :left-bracket)
            (single-char-matcher \} :right-bracket)
            (single-char-matcher \, :comma)
            (single-char-matcher \. :dot)
            (single-char-matcher \- :minus)
            (single-char-matcher \+ :plus)
            (single-char-matcher \; :semicolon)
            (single-char-matcher \* :star)
            (double-char-matcher \= \= :equal :equal-equal)
            (double-char-matcher \! \= :bang :bang-equal)
            (double-char-matcher \< \= :less :less-equal)
            (double-char-matcher \> \= :greater :greater-equal)]))


(defn number-token [state]
  "Create a number token from the current state"
  (let [start-position (:current state)]
    (loop [state state
           number (str (get-current-char state))
           passed-point false]
      (let [next-char (get-next-char state)]
        (cond
          (digit? next-char) (recur (increase-current state) (str number next-char) passed-point)

          (= \. next-char) (if passed-point
                             (println "TODO: Handle error")
                             (recur (increase-current state) (str number next-char) true))

          :else {:type     :number
                 :lexeme   (Double/parseDouble number)
                 :position start-position
                 :size     (count number)})))))


(defn identifier [state]
  "Get the next identifier from the current state"
  (loop [state state
         keyword (str (get-current-char state))]
    (let [next-char (get-next-char state)]
      (if (or (digit? next-char)
              (alpha? next-char))
        (recur (increase-current state)
               (str keyword next-char))
        keyword))))

(defn identifier-token [state]
  "Create either a builtin identifier token, or if its a builtin
   a specific one"
  (if-let [identifier (identifier state)]
    (if (contains? builtin-keyword-map identifier)
      {:type     (builtin-keyword-map identifier)
       :lexeme   identifier
       :position (:current state)
       :size     (count identifier)}
      {:type     :identifier
       :lexeme   identifier
       :position (:current state)
       :size     (count identifier)})))


(defn string [state]
  "Get the next string from the current position"
  (if (= \" (get-current-char state))
    (loop [state (increase-current state)
           string-builder ""]
      (let [current-char (get-current-char state)]
        (if (= \" current-char)
          string-builder
          (recur (increase-current state)
                 (str string-builder current-char)))))))



(defn string-token [state]
  (if-let [parsed-string (string state)]
    {:type :string
     :lexeme parsed-string
     :position (:current state)
     :size (+ 2 (count parsed-string))}))

(defn comment-string [state]
  "Reads till there is a next line or the is nothign left"
  (loop [state state
         comment-string ""]
    (let [current-char (get-current-char state)]
      (if (or
            (at-end? state)
            (newline? current-char))
        comment-string
        (recur (increase-current state) (str comment-string current-char))))))

(defn comment-token [state]
  "Create a comment token"
  (let [comment (comment-string state)]
    {:type :comment
     :lexeme comment
     :size (count comment)
     :position (:current state)}))

(defn current-token [state]
  "Returns the token at the current and the amount to increase with"
  (let [current-char (get-current-char state)]
    (cond
      (contains? char->token-factory current-char) (let [lookup-result (char->token-factory current-char)]
                                                     (lookup-result state))
      (= \/ current-char) (let [next-char (get-next-char state)]
                            (if (= \/ next-char)
                              (comment-token state)
                              ((define-single-char-token \/ :slash) state)))

      (= \" current-char) (string-token state)
      (digit? current-char) (number-token state)
      (alpha? current-char) (if-let [keyword (identifier-token state)]
                              keyword
                              (println "Todo: error handling for unkown keywords"))
      :else {:type     :unknown
             :lexeme   (str current-char)
             :position (:current state)
             :size     1})))

(defn insert-token [state token]
  (update state :tokens #(conj % token)))

(defn remove-whitespace [state]
  (loop [state state]
    (let [current-char (get-current-char state)]
      (if (whitespace? current-char)
        (recur (increase-current state))
        state))))

(defn add-current-token [state]
  (let [token (current-token state)]
    (-> state
        (insert-token token)
        (increase-current (:size token)))))

(defn program->tokens [program]
  "Convert a program into tokens"
  (loop [state {:tokens  []
                :program program
                :current 0
                :line    1}]
    (let [current-char (get-current-char state)]
      (cond
        (whitespace? current-char) (recur (remove-whitespace state))
        (newline? current-char) (recur (update state :line inc))
        :else (let [next-state (add-current-token state)]
                (if (at-end? next-state)
                  (:tokens next-state)
                  (recur next-state)))))))