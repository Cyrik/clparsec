(ns clparsec.demos.tutorial
  (:use [clojure.algo.monads][clparsec.core][clparsec.primitives][clparsec.char-parsers])
  (:require [clojure [set :as set]])
  (:import [clparsec.errors ErrorMessage]
           [Character])
  (:refer-clojure :exclude #{newline}))

(defn test-parse [p str]
  (let [reply (run p str)]
    (if (success? reply)
      (println "Success:" (result reply))
      (println "Failure:" (.toString (errors reply))))))

(test-parse pfloat "1.25")

(test-parse pfloat "1.25E 3")


;;;;;;;;; parsing floats between brackets ;;;;;;;;;;;;;;
(def float-between-brackets (|>> (>>| (pstring "[") pfloat) (pstring "]")))

(test-parse float-between-brackets "[1.0]")

(test-parse float-between-brackets "[]")

(test-parse float-between-brackets "[1.0")

;;;;;;;;;;;;;; abstracting parsers ;;;;;;;;;;;;;;;;

