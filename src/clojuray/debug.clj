(ns clojuray.debug)

; DEBUG MACRO: debug let.
; Print out all of the values in the let
(defmacro dlet [bindings & body]
  `(let [~@(mapcat (fn [[n v]]
                     (if (or (vector? n) (map? n))
                       [n v]
                       [n v '_ `(println (name '~n) " : " ~v)]))
                   (partition 2 bindings))]
     ~@body))
