(ns clparsec.char-parsers
  (:use [clojure.algo.monads][clparsec.core][clparsec.primitives])
  (:require [clojure [set :as set]])
  (:import [clparsec.core ErrorMessage]
           [Character])
  (:refer-clojure :exclude #{newline}))



(defn expected-string [str]
  (ErrorMessage. :expected-string str))
    
(def expected-any-char
  (ErrorMessage. :expected-any-char "any character"))

(defn expected-any-char-in [s]
  (ErrorMessage. :expected-any-char-in (str s)))

(defn expected-any-char-not-in [s]
  (ErrorMessage. :expected-any-char-not-in (str s)))

(defn newline-return [result]
  (fn [state]
    (if-let [state2 (skip-newline state)]
      (make-success state2 result)
      (make-failure state (make-parse-error state (expected "newline"))))))

(def pnewline (newline-return \newline))

(def skip-nl (newline-return nil))

(defn char-returnE [c result error]
  (fn [state]
    (if-let [new-state (skip state c)]
      (make-success new-state result)
      (make-failure state (make-parse-error state error)))))

(defn char-return [c result]
  (condp contains? c
    *newline-chars* (newline-return result)
    #{\uFFFF} (throw (IllegalArgumentException. "\uffff (EOS) is not a valid char for pchar/skip-char/char-return"))
    (char-returnE c result (expected-string c))))
  
(defn pchar [c]
  (char-return c c))

(defn skip-char [c] 
  (char-return c nil))

(defn any-char [state]
    (if-let [[s c] (read-char-or-newline state)]
      (make-success s c)
      (make-failure state (make-parse-error state expected-any-char))))
      
(defn skip-any-char [state]
    (if-let [[s c] (read-char-or-newline state)]
      (make-success s nil)
      (make-failure state (make-parse-error state expected-any-char))))


(defn- satisfyE [pred e-msg]
  (fn [state]
    (if-let [c (peep state)]
      (case c
        (\return \newline) (if (pred \newline)
                             (make-success (skip-newline state) \newline)
                             (make-failure state (make-parse-error state e-msg)))
        (if (and (not= c \uffff) (pred c))
          (make-success (skip-one state) c)
          (make-failure state (make-parse-error state e-msg))))
      (make-failure state (make-parse-error state e-msg)))))

(defn- skip-satisfyE [pred e-msg]
  (fn [state]
    (if-let [c (peep state)]
      (case c
        (\return \newline) (if (pred \newline)
                             (make-success (skip-newline state) nil)
                             (make-failure state (make-parse-error state e-msg)))
        (if (and (not= c \uffff) (pred c))
          (make-success (skip-one state) nil)
          (make-failure state (make-parse-error state e-msg))))
      (make-failure state (make-parse-error state e-msg)))))

(defn satisfy [pred]
  (satisfyE pred nil))

(defn satisfyL [pred label]
  (satisfyE pred (expected label)))

(defn skip-satisfy [pred]
  (skip-satisfyE pred nil))

(defn skip-satisfyL [pred label]
  (skip-satisfyE pred (expected label)))

(defn any-of [& chars]
  (let [chars (set chars)]
    (satisfyE chars (expected-any-char-in chars))))

(defn any-of-set [chars]
  (satisfyE chars (expected-any-char-in chars)))

(defn skip-any-of [& chars]
  (let [chars (set chars)]
    (skip-satisfyE chars (expected-any-char-in chars))))

(defn skip-any-of-set [chars]
  (skip-satisfyE chars (expected-any-char-in chars)))

(defn none-of [& chars]
  (let [chars (set chars)]
    (satisfyE (comp not chars) (expected-any-char-not-in chars))))

(defn none-of-set [chars]
  (satisfyE (comp not chars) (expected-any-char-not-in chars)))

(defn skip-none-of [& chars]
  (let [chars (set chars)]
    (skip-satisfyE (comp not chars) (expected-any-char-not-in chars))))

(defn skip-none-of-set [chars]
  (skip-satisfyE (comp not chars) (expected-any-char-not-in chars)))

(defn- char-lte? [a b]
  (<=(.compareTo a b) 0)) 
(defn- char-gte? [a b]
  (>= (.compareTo a b) 0))


(defn is-ascii-upper? [c]
  (and (char-gte? c \A) (char-lte? c \Z)))

(defn is-ascii-lower? [c]
  (and (char-gte? c \a) (char-lte? c \z)))

(defn is-digit? [c]
  (and (char-gte? c \0) (char-lte? c \9)))

(defn is-hex? [c]
  (or (and (char-gte? c \0) (char-lte? c \9))) (and (char-gte? c \A) (char-lte? c \F)))
(defn is-ascii-letter? [c]
  (or (is-ascii-upper? c) (is-ascii-lower? c)))

(def ascii-upper (satisfyE is-ascii-upper? (expected "ascii-uppercase-letter")))
(def ascii-lower (satisfyE is-ascii-lower? (expected "ascii-lowercase-letter")))
(def ascii-letter (satisfyE is-ascii-letter? (expected "ascii-letter")))

(defn is-upper-case? [c]
  (Character/isUpperCase c))
(defn is-lower-case? [c]
  (Character/isLowerCase c))
(defn is-letter? [c]
  (Character/isLetter c))

(def upper (satisfyE is-upper-case? (expected "uppercase-letter")))
(def lower (satisfyE is-lower-case? (expected "lowercase-letter")))
(def letter (satisfyE is-letter? (expected "letter")))

(def digit (satisfyE is-digit? (expected "decimal-digit")))
(def hex (satisfyE is-hex? (expected "hexadecimal-digit")))

(def tab (satisfyE #(= % \tab) (expected "tab")))

(defn spaces [state]
  (if-let [s (skip-whitespace state)]
    (make-success s)
    (make-success state)))

(defn spaces1 [state]
  (if-let [s (skip-whitespace state)]
    (make-success s)
    (make-failure state(make-parse-error state (expected "whitespace")))))

(defn eof [state]
  (if (end? state)
    (make-success state)
    (make-failure state (make-parse-error state (expected "end of file")))))

(defn- find-newline-or-eos [s]
  (let [newline-eos (conj *newline-chars* \uFFFF)] ;buggy?
    (loop [i (dec (count s))]
      (if (>= i 0)
        (if (newline-eos(nth s i))
          i
          (recur (dec i)))))))
  
(defn string! [strn result]
  (if-let [newline (find-newline-or-eos strn)]
    (throw (IllegalArgumentException. "string! may not contain newline in string"))
    (fn [state]
      (if-let [s (skip-strn state strn)]
        (make-success s  result)
        (make-failure state (make-parse-error state (expected-string strn)))))))

(defn pstring [s] (string! s s))
(defn skip-string [s] (string! s nil))

(defn- many-satisfy-internal [require1 f1 f2 error]
  (fn [state]
    (let [[result state2] (read-chars-or-newlines-while state f1 f2 true)]
      (if (and require1 (not result))
        (make-failure state (make-parse-error state error))
        (make-success state2 result)))))

(defn many-satisfy2 [f1 f]
  (many-satisfy-internal false f1 f nil))
(defn many1-satisfy2 [f1 f]
  (many-satisfy-internal true f1 f nil))
(defn many1-satisfy2L [f1 f label]
  (many-satisfy-internal true f1 f (expected label)))

(defn many-satisfy [f]
  (many-satisfy2 f f))
(defn many1-satisfy [f]
  (many1-satisfy2 f f))
(defn many1-satisfyL [f label]
  (many1-satisfy2L f f label))



