(ns user
  (:require [clojure.spec.alpha :as s]
            [blockchain.core]
            [expound.alpha :as expound]
            [orchestra.spec.test :as st]))

(set! s/*explain-out* expound/printer)
(st/instrument)
