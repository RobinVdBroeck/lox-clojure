(ns lox.scanner-test
  (:require [clojure.test :refer :all]
            [lox.scanner :refer :all]))

(defn create-state
  ([program] (create-state program 0))
  ([program current] {:tokens  '()
                      :program program
                      :current current
                      :line    1}))

(deftest is-at-end-tests
  (testing "Returns false when not at end"
    (let [not-at-end (create-state "aa" 0)]
      (is (not (at-end? not-at-end)))))
  (testing "Return true when at end"
    (let [at-end-state (create-state "aa" 2)]
      (is (at-end? at-end-state)))))

(deftest number-token-tests
  (testing "Numbers without points (integers)"
    (let [state (create-state "5")]
      (is (= 5.0
             (:lexeme (number-token state))))))
  (testing "Numbers with 1 point in them"
    (let [state (create-state "5.5")]
      (is (= 5.5
             (:lexeme (number-token state))))))
  (testing "Numbers ending in a point"
    (let [state (create-state "5.")]
      (is (= 5.0
             (:lexeme (number-token state)))))))

(deftest get-string-string
  (testing "Gets a plain string"
    (let [state (create-state "test")]
      (is (= "test" (identifier state)))))
  (testing "Gets a string with a number in it"
    (let [state (create-state "test9")]
      (is (= "test9" (identifier state))))))

(deftest identifier-token-tests
  (testing "If there is no matching keyword, return nil"
    (let [unkown-keyword "UNKNOWN"
          state (create-state unkown-keyword)]
      (is (= {:type :identifier
              :lexeme unkown-keyword
              :position 0
              :size (count unkown-keyword)}
             (identifier-token state)))))
  (testing "If there is a matching keyword create a token"
    (let [state (create-state "if")]
      (is (= {:type     :if
              :lexeme   "if"
              :position 0
              :size     2}
             (identifier-token state))))))



(deftest program->tokens-tests
  (testing "Should detect parens and brackets"
    (let [tokens (program->tokens "({})")]
      (is (=
            [{:type     :left-paren
              :lexeme   "("
              :position 0
              :size     1}
             {:type     :left-bracket
              :lexeme   "{"
              :position 1
              :size     1}
             {:type     :right-bracket
              :lexeme   "}"
              :position 2
              :size     1}
             {:type     :right-paren
              :lexeme   ")"
              :position 3
              :size     1}]
            tokens)))))

