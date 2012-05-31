(ns clparsec.test.char-parsers
  (:use [clparsec core char-parsers errors])
  (:use [clojure.test])
  (:use [clparsec.test.helpers])
  (:import (clparsec.core Reply)))

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
  (rfail (satisfyL #(= % \return) "test") "\r" 0 #{(expected "test")})
  (roknl (satisfy #(= % \newline)) "\r" 1 \newline)
  (roknl (satisfy #(= % \newline)) "\r\n" 2 \newline)
  (roknl (satisfy #(= % \newline)) "\n" 1 \newline))

(deftest test-skip-satisfy
  (rok (skip-satisfy #(= % \1)) "1" 1 nil)
  (rok (skip-satisfy #(= % \tab)) "\t" 1 nil)
  (rok (skip-satisfy #(= % \1)) "11" 1 nil)
  (rfail (skip-satisfy #(= % \1)) "0" 0 #{nil})
  (rfail (skip-satisfyL #(= % \1) "test") "2" 0 #{(expected "test")})
  (rfail (skip-satisfyL #(= % \return) "test") "\r" 0 #{(expected "test")})
  (roknl (skip-satisfy #(= % \newline)) "\r" 1 nil)
  (roknl (skip-satisfy #(= % \newline)) "\r\n" 2 nil)
  (roknl (skip-satisfy #(= % \newline)) "\n" 1 nil))

(deftest test-any-of
  (rok (any-of \1 \2) "12" 1 \1)
  (rok (any-of-set #{\1 \2}) "12" 1 \1)
  (roknl (any-of \1 \newline) "\r" 1 \newline)
  (roknl (any-of \1 \newline) "\r\n" 2 \newline)
  (rfail (any-of \1 \2) "3" 0 #{(expected-any-char-in #{\1 \2})})
  (rfail (any-of \1 \return) "\r" 0 #{(expected-any-char-in #{\1 \return})})
  (rfail (any-of-set #{\1 \2}) "3" 0 #{(expected-any-char-in #{\1 \2})}))

(deftest test-none-of
  (rok (none-of \1 \2) "3" 1 \3)
  (rok (none-of-set #{\1 \2}) "3" 1 \3)
  (rok (none-of \1 \2) "\r" 1 \newline)
  (rok (none-of \1 \2) "\r\n" 2 \newline)
  (rfail (none-of \1 \2) "1" 0 #{(expected-any-char-not-in #{\1 \2})})
  (rfail (none-of \1 \newline) "\n" 0 #{(expected-any-char-not-in #{\1 \newline})})
  (rfail (none-of-set #{\1 \2}) "1" 0 #{(expected-any-char-not-in #{\1 \2})}))

(deftest test-skip-any-of
  (rok (skip-any-of \1 \2) "12" 1 nil)
  (rok (skip-any-of-set #{\1 \2}) "12" 1 nil)
  (roknl (skip-any-of \1 \newline) "\r" 1 nil)
  (roknl (skip-any-of \1 \newline) "\r\n" 2 nil)
  (rfail (skip-any-of \1 \2) "3" 0 #{(expected-any-char-in #{\1 \2})})
  (rfail (skip-any-of \1 \return) "\r" 0 #{(expected-any-char-in #{\1 \return})})
  (rfail (skip-any-of-set #{\1 \2}) "3" 0 #{(expected-any-char-in #{\1 \2})}))

(deftest test-skip-none-of
  (rok (skip-none-of \1 \2) "3" 1 nil)
  (rok (skip-none-of-set #{\1 \2}) "3" 1 nil)
  (rok (skip-none-of \1 \2) "\r" 1 nil)
  (rok (skip-none-of \1 \2) "\r\n" 2 nil)
  (rfail (skip-none-of \1 \2) "1" 0 #{(expected-any-char-not-in #{\1 \2})})
  (rfail (skip-none-of \1 \newline) "\n" 0 #{(expected-any-char-not-in #{\1 \newline})})
  (rfail (skip-none-of-set #{\1 \2}) "1" 0 #{(expected-any-char-not-in #{\1 \2})}))

(deftest test-ascii-parsers
  (rok ascii-upper "A" 1 \A)
  (rok ascii-lower "a" 1 \a)
  (rok ascii-letter "A" 1 \A)
  (rfail ascii-upper "z" 0 #{(expected "ascii-uppercase-letter")})
  (rfail ascii-lower "A" 0 #{(expected "ascii-lowercase-letter")})
  (rfail ascii-letter "1" 0 #{(expected "ascii-letter")}))

(deftest test-letter-parsers
  (rok upper "Ä" 1 \Ä)
  (rok lower "ä" 1 \ä)
  (rok letter "Ü" 1 \Ü)
  (rfail upper "ä" 0 #{(expected "uppercase-letter")})
  (rfail lower "Ä" 0 #{(expected "lowercase-letter")})
  (rfail letter "1" 0 #{(expected "letter")}))

(deftest test-digit-parsers
  (rok digit "1" 1 \1)
  (rok hex "F" 1 \F)
  (rfail digit "z" 0 #{(expected "decimal-digit")})
  (rfail hex "G" 0 #{(expected "hexadecimal-digit")}))

(deftest test-tab
  (rok tab "\t" 1 \tab)
  (rfail tab "a" 0 #{(expected "tab")}))

(deftest test-spaces
  (rok spaces "" 0 nil)
  (rok spaces " " 1 nil)
  (rok spaces "  " 2 nil)
  (rfail spaces1 "" 0 #{(expected "whitespace")})
  (rok spaces1 " " 1 nil)
  (rok spaces1 "  " 2 nil))

(deftest test-spaces-with-newlines
  (let [res (run spaces "\n \r\t\t\r\n\n ")]
    (is (= (position(:state res))9))
    (is (= (:line (location (:state res))) 5))
    (is (= (:column (location (:state res))) 2))))

(deftest test-spaces1-with-newlines
  (let [res (run spaces1 "\n \r\t\t\r\n\n ")]
    (is (= (position(:state res))9))
    (is (= (:line (location (:state res))) 5))
    (is (= (:column (location (:state res))) 2))))

(deftest test-eof
  (rok eof "" 0 nil)
  (rfail eof "1" 0 #{(expected "end of file")}))

(deftest test-empty-pstring-parser
  (rok (pstring "") "1" 0 ""))

(deftest test-pstring-basics
  (rok (pstring "1") "1" 1 "1")
  (rfail (pstring "1") "" 0 #{(expected-string "1")})
  (rfail (pstring "1") "2" 0 #{(expected-string "1")}))

(deftest test-pstring-lenght2
  (rfail (pstring "12") "" 0 #{(expected-string "12")})
  (rfail (pstring "12") "1" 0 #{(expected-string "12")})
  (rfail (pstring "12") "22" 0 #{(expected-string "12")})
  (rfail (pstring "12") "13" 0 #{(expected-string "12")})
  (rok (pstring "12") "1212" 2 "12"))

(deftest test-pstring-doesnt-allow-newlines-eof
  (do
    (is (thrown? IllegalArgumentException (pstring "\r")))
    (is (thrown? IllegalArgumentException (pstring "\n")))
    (is (thrown? IllegalArgumentException (pstring "\uffff")))
    (is (thrown? IllegalArgumentException (pstring "\r1")))
    (is (thrown? IllegalArgumentException (pstring "1\n")))
    (is (thrown? IllegalArgumentException (pstring "12\n")))))

(deftest test-many-satisfy
  (rok (many-satisfy is-digit?) "" 0 "")
  (rok (many-satisfy2 is-digit? is-hex?) "" 0 "")
  (rok (many-satisfy is-digit?) "123a" 3 "123")
  (rok (many-satisfy2 is-hex? is-digit? ) "A123123z" 7 "A123123"))

(deftest test-number-literal
  (run (number-literal #{} "explo")  "123A"))
