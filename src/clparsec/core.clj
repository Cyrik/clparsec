(ns clparsec.core
  (:use clojure.algo.monads)
  (:require [clojure.set :as set]))

(def ^:dynamic *newline-chars*
  #{\return \newline "\r\n"})

(def ^:dynamic *whitespace*
  #{\space \tab \newline \return \formfeed})

(defprotocol AReply
  (success? [r])
  (result [r])
  (errors [r]))

(defrecord Reply [status result errors state]
  AReply
  (success? [this] (if (= status :success) true false))
  (result [this] result)
  (errors [this] errors))

(defprotocol ALocation
  (location-code [location]))

(extend-protocol ALocation
  Integer
  (location-code [position] (format "position %s" position)))

(defrecord LineLocation [line]
  ALocation
  (location-code [this] (format "line %s" line)))

(defprotocol ALineAndColumnLocation
  (inc-line [location])
  (inc-column [location])
  (plus-column [location n])
  (plus-line [location n]))

(defrecord StandardLocation [line column]
  ALocation
  (location-code [this] (format "line %s, column %s" line column))

  ALineAndColumnLocation
  (inc-line [this] (assoc this :line (inc line), :column 1))
  (inc-column [this] (assoc this :column (inc column)))
  (plus-column [this n] (assoc this :column (+ column n)))
  (plus-line [this n] (assoc this :line (+ line n))))

(defn make-standard-location [line column]
  {:pre #{(integer? line) (integer? column)}}
  (StandardLocation. line column))

(defn standard-alter-location [character]
  (if (*newline-chars* character)
    inc-line inc-column))

(defprotocol AState
  "The protocol of FnParse states, which must
  be able to return a position."
  (get-remainder [state])
  (position [state])
  (skip-one [state])
  (skip [state c])
  (next-state [state])
  (location [state])
  (peep [state])
  (skip-whitespace [state])
  (skip-strn [state strn])
  (skip-and-peek [state])
  (read-chars-or-newlines-while [state pred1 pred normalize-n])
  (end? [state])
  (skip-newline [state])
  (read-char-or-newline [state])
  (user-state [state]))

(defrecord State [remainder position location user-state]
  AState
  (position [this] position)

  (get-remainder [this] remainder)

  (user-state [this] user-state)

  (location [this] location)

  (end? [this] (empty? remainder))

  (next-state [this]
    (when-let [remainder (seq remainder)]
      (assoc this
        :remainder (next remainder), :position (inc position),
        :location ((standard-alter-location (first remainder)) location))))

  (skip-and-peek [this] (let [next-state (next-state this)]
                          (list (peep next-state) next-state)))

  (skip [this c] (when (=(first remainder)c)
                   (next-state this)))
  (skip-newline [this]
    (if (= \return (first remainder))
      (if (= \newline (second remainder))
        (assoc this
          :remainder (next (next remainder)), :position (+ position 2)
          :location ((standard-alter-location "\r\n") location))
        (next-state this))
      (if (*newline-chars* (first remainder))
        (next-state this))))

  (read-char-or-newline [this]
    (if-let [c (first remainder)]
      (if (= \return c)
        (if (= \newline (second remainder))
          (list (assoc this
                  :remainder (next (next remainder)), :position (+ position 2)
                  :location ((standard-alter-location "\r\n") location))
                \newline)
          (list (next-state this) \newline))
        (list (next-state this) c))))

  (peep [this] (if-let [res(first remainder)]
                 res
                 \uFFFF))

  (skip-one [this] (next-state this))

  (skip-whitespace [this]
    (when (*whitespace* (first remainder))
      (loop [state this]
        (let [c (peep state)]
          (if (= \return c)
            (if (= \newline (second (:remainder state)))
              (recur (assoc state
                       :remainder (next (next (:remainder state))), :position (+ (:position state) 2)
                       :location ((standard-alter-location "\r\n") (:location state))))
              (recur (next-state state)))
            (if (*whitespace* c)
              (recur (next-state state))
              state))))))

  (skip-strn [this strn]
    (let [length (count strn)]
      (when (<= length (count remainder))
        (loop [i 0]
          (if (< i length)
            (when (=(nth strn i) (nth remainder i))
              (recur (inc i)))
            (assoc this :remainder (drop i remainder)
                   :position (+ position i)
                   :location (plus-column location i)))))))

  (read-chars-or-newlines-while [this pred1 pred normalize-n] ;todo normalize newlines
    (if-let [c (first remainder)]
      (if (pred1 c)
        (loop[state (next-state this)
              acc [(first remainder)]]
          (if-let [frst (peep state)]
            (if (pred frst)
              (recur (next-state state) (conj acc frst))
              (list (apply str acc) state))
            (list (apply str acc) state)))
        (list "" this))
      (list "" this)))
) ;; end of State defrecord

(defn make-state
  "Creates a state with the given parameters."
  [input & {:keys #{location user-state}
            :or {location (make-standard-location 1 1)}}]
  (State. input 0 location user-state))

(defn make-failure
  ([state] (Reply. :fail nil nil state))
  ([state errors] (Reply. :fail nil errors state)))

(defn make-fatal-error
  ([state] (Reply. :fatal nil nil state))
  ([state errors] (Reply. :fatal nil errors state)))

(defn make-success
  ([state] (Reply. :success nil nil state))
  ([state result] (Reply. :success result nil state))
  ([state result errors] (Reply. :success result errors state)))

(defn run [p str]
  (p (make-state str)))
