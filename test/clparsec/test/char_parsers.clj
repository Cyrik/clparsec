(ns clparsec.test.char-parsers
  (:use [clparsec.char-parsers])
  (:use [clojure.test])
  (:use [clparsec.core])
  (:import (clparsec.core Reply)))

(defn result-success-internal [with-newline content nskipped-chars result errors parser]
  (let [state (make-state content)
        res (parser state)]
    (do 
      (is (success? res))
      (is (= (:result res) result))
      (is (= (:messages (:errors res)) errors))
      (is (= (get-position (:state res)) nskipped-chars))
      (if with-newline
        (is (=(:line (location (:state res)))1))))))

(defn rok 
  "result ok"
  [parser content n-skipped-chars result]
  (result-success-internal false content n-skipped-chars result nil parser))

(defn roke 
  "result ok with error messages"
  [parser content n-skipped-chars result errors]
  (result-success-internal false content n-skipped-chars result errors parser))

(defn roknl
  "result ok with newline"
  [parser content n-skipped-chars result ]
  (result-success-internal true content n-skipped-chars result nil parser))

(defn result-failure-internal [parser status content n-skipped-chars errors]
  (let [state (make-state content)
        res (parser state)]
    (do
      (is (= (:status res) status))
      (is (= (:messages (:errors res)) errors))
      (is (= (get-position (:state res)) n-skipped-chars)))))

(defn rfail [parser content n-skipped-chars errors]
  (result-failure-internal parser :fail content n-skipped-chars errors))
          

(deftest test-basics
  (rok (pchar \space) " " 1 \space )
  (rok (pchar \tab) "\t\t" 1 \tab )
  (rfail (pchar \space) "" 0 #{(expected-string \space)})
  (rfail (pchar \space) "x" 0 #{(expected-string \space)}))

(deftest test-newlines-fail
  (rfail (pchar \return) "_\r" 0 #{(expected "newline")})
  (rfail pnewline "_\n" 0 #{(expected "newline")})
  (rfail pnewline "" 0 #{(expected "newline")}))

(deftest test-newlines-ok  
  (roknl pnewline "\r" 1 \newline)
  (roknl (pchar \newline) "\r" 1 \newline)
  (roknl (pchar \return) "\r" 1 \return)
  (roknl (pchar \newline) "\r\n" 2 \newline)
  (roknl (pchar \return) "\r\n" 2 \return)
  (roknl (pchar \newline) "\n" 1 \newline)
  (roknl (pchar \return) "\n" 1 \return))

(deftest test-char-return  
  (rok (skip-char \tab) "\t" 1 nil)
  (rok (char-return \tab 0) "\t" 1 0)
  (roknl skip-nl "\n" 1 nil)
  (roknl (newline-return 0) "\r\n" 2 0) 
  
  (is (thrown? IllegalArgumentException (pchar \uffff))))

(deftest test-any-char
  (rfail any-char "" 0 #{expected-any-char})
  (rfail skip-any-char "" 0 #{expected-any-char})
  
  (rok any-char " " 1 \space)
  (rok any-char "\ufffe" 1 \ufffe)
  (rok skip-any-char " " 1 nil)
  (rok any-char "\t\t" 1 \tab)
  (rok skip-any-char "\t\t" 1 nil))

(deftest test-any-char-newline
  (roknl any-char "\r\n" 2 \newline)
  (roknl skip-any-char "\r\n" 2 nil)
  (roknl any-char "\n\n" 1 \newline)
  (roknl skip-any-char "\n\r" 1 nil))

(deftest test-satisfy-errors
  (rfail (satisfy #(%)) "" 0 #{nil})
  (rfail (skip-satisfy #(%)) "" 0 #{nil})
  (rfail (satisfyL #(%) "test") "" 0 #{(expected "test")})
  (rfail (skip-satisfyL #(%) "test") "" 0 #{(expected "test")}))

(deftest test-satisfy
  (rok (satisfy #(= % \1)) "1" 1 \1)
  (rok (satisfy #(= % \tab)) "\t" 1 \tab)
  (rok (satisfy #(= % \1)) "11" 1 \1)
  (rfail (satisfy #(= % \1)) "0" 0 #{nil})
  (rfail (satisfyL #(= % \1) "test") "2" 0 #{(expected "test")})
  (rfail (satisfyL #(= % \return) "test") "0" 0 #{(expected "test")}))

            