(ns blockchain.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            )
  )


(s/def :bc/sha (s/and string?
                      #(re-matches #"[A-Fa-f0-9]+" %)))

(s/def :bc/sender :bc/sha)
(s/def :bc/recipient :bc/sha)
(s/def :bc/amount number?)
(s/def :bc/proof pos-int?)
(s/def :bc/prev-hash :bc/sha)

(s/def :bc/transaction (s/keys :req
                               [:bc/sender
                                :bc/recipient
                                :bc/amount]
                               ))
(s/def :bc/transactions (s/coll-of :bc/transactions
                                   :kind vector?))

(s/def :bc/block (s/keys :req
                         [:bc/index
                          :bc/timestamp
                          :bc/transactions
                          :bc/proof
                          :bc/prev-hash]
                         ))

(s/def :bc/chain (s/coll-of :bc/block :kind vector?))

(s/def :bc/bc (s/keys :req [:bc/transactions
                             :bc/chain]))

(s/fdef add-tx
        :args (s/cat :bc :bc/bc
                     :sender :bc/sender
                     :recipient :bc/recipient
                     :amount :bc/amount)
        :ret :bc/bc)
(defn add-tx [bc sender recipient amount]
  
  )

(comment
  (require '[orchestra.spec.test :as st])
  (set! s/*explain-out* expound/printer)
  (st/instrument)
  
  (add-tx nil nil nil nil)
  )


