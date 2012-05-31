(ns clparsec.char-parsers
  (:use [clojure.algo.monads]
        [clparsec core primitives errors])
  (:require [clojure.set :as set])
  (:import [clparsec.errors ErrorMessage]
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

(def pdigits (many-satisfy is-digit?))
(defn- psign [options c state]
  (case c
    \+ (if (:allow-plus options)
         (list \+ (next-state state) #{:has-plus-sign})
         (list nil state #{}))
    \- (if (:allow-minus options)
         (list \- (next-state state) #{:has-plus-sign})
         (list nil state #{}))
    (list nil state #{})))

(defn- p-exp-sign [state]
  (case (peep state)
    \+ (list \+ (next-state state))
    \- (list \- (next-state state))
    (list "" state)))

(defn- pfraction-part [options state flags]
  (if (and (= (peep state) \.) (:allow-fraction options))
    (let [flags (conj flags :has-fraction)
          [c next-state] (skip-and-peek state)]
      (if (is-digit? c)
        (let [pdigit-res (pdigits next-state)]
          {:errors nil, :state (:state pdigit-res),:flags flags, :result (str \. (:result pdigit-res))})
        {:errors (expected "decimal digit"), :state next-state, :flags flags, :result "."}))
    {:errors nil, :state state, :flags flags, :result ""}))

(defn- pexponent-part [options state flags errors]
  (let [c (peep state)]
    (if (and (or (= c \e) (= c \E))
             (not errors)
             (:allow-exponent options))
      (let [flags (conj flags :has-exponent)
            next-state (skip-one state)
            [e-sign next-state] (p-exp-sign next-state)]
        (if (is-digit? (peep next-state))
          (let [pdigit-res (pdigits next-state)]
            {:errors nil, :state (:state pdigit-res),
             :flags flags, :result (str c e-sign (:result pdigit-res))})
          {:errors (expected "decimal digit"), :state next-state, :flags flags, :result (str c e-sign)}))
      {:errors errors, :state state, :flags flags, :result ""})))

(defn- parse-decimal-literalE [options state flags]
  (let [flags (conj flags :is-decimal)
        integer-part-res (pdigits state)
        integer-part (:result integer-part-res)
        flags (if integer-part (conj flags :has-integer-part) flags)
        fraction-part-res (pfraction-part options (:state integer-part-res) flags)
        errors (:errors fraction-part-res)
        fraction-state(:state fraction-part-res)
        exp-part-res (pexponent-part options fraction-state flags errors)
        errors (set/union errors (:errors exp-part-res))
        flags (set/union flags (:flags fraction-part-res) (:flags exp-part-res))
        result (str integer-part (:result fraction-part-res) (:result exp-part-res))]
    {:errors errors, :flags flags, :result result, :state (:state exp-part-res)}))

(defn parse-other-number-literalE [options state e-msg sign]
  (throw (UnsupportedOperationException. "not implemented yet")))

(defn parse-number-literalE [options state e-msg sign flags]
  (let [frst (peep state)
        next-state (next-state state)
        scnd (peep next-state)]
    (if (or (not= frst \0)
            (char-lte? scnd \9)
            (not (some options [:allow-binary :allow-hexadecimal :allow-octal]))
            (or (= scnd \e) (= scnd \E)))
      (parse-decimal-literalE options state flags)
      (parse-other-number-literalE options state flags))))

(defn prepare-result [result options]
  (let [state (:state result)
        pascii (many-satisfy is-ascii-letter?)]
    (if-not (:errors result)
      (if-not (and (:allow-suffix options) (is-ascii-letter? (peep (:state result))))
        (make-success state {:string (:result result), :flags (:flags result), :suf1 \uFFFF, :suf2 \uFFFF, :suf3 \uFFFF, :suf4 \uFFFF})
        (throw (UnsupportedOperationException. "not implemented yet")))
      (make-failure state (make-parse-error state (:errors result))))))

(defn number-literalE [options e-msg]
  (fn [state]
    (if-let [frst (peep state)] ;can this be a let?
      (let [[sign new-state flags] (psign options frst state)]
        (let [scnd (peep new-state)]
          (if (or (is-digit? scnd)
                  (and (= scnd \.) (:allow-fraction options) (:allow-fraction-wo-integer options)))
            (prepare-result (parse-number-literalE options new-state e-msg sign flags) options)
            (if false;(or check-infi check-nan)
              (make-success state);infi or check-nan)
              (make-failure state (make-parse-error state e-msg))))))
      (make-failure state (make-parse-error state e-msg)))))

(def default-float #{:allow-minus :allow-plus :allow-fraction
                     :allow-exponent :allow-hexadecimal :allow-infinity :allow-nan})

(defn number-literal [options label]
  (number-literalE options label))

(def ^{:private true} fp-error-msg
  "The floating-point number has an invalid format (this error is unexpected, please report this error message to https://github.com/Cyrik/clparsec/issues).")

(defn pfloat [state]
  (let [reply ((number-literal default-float (expected "floating point number")) state)]
    (if (success? reply)
      (let [nl (:result reply)]
        (try
          (if (:is-decimal (:flags nl))
            (make-success (:state reply) (Double/valueOf (:string nl))))
          (catch NumberFormatException e
            (make-fatal-error (:state reply)
                              (make-parse-error (:state reply) (message-error fp-error-msg))))))
      reply)))
