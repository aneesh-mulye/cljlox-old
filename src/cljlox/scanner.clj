(ns cljlox.scanner)

(def unichar
  {\( :left-paren
   \) :right-paren
   \{ :left-brace
   \} :right-brace
   \, :comma
   \. :dot
   \- :minus
   \+ :plus
   \; :semicolon
   \* :star
  })

(def ambig-twochar
  {"!=" :bang-equal
   "==" :equal-equal
   "<=" :less-equal
   ">=" :greater-equal})

(def ambig-unichar
  {\! :bang
   \= :equal
   \< :less
   \> :greater})

;; TODO: add error handling! Return a map with parsed tokens,
;; and encountered errors with line numbers, rather than the current
;; assumption of working only on valid code.
(defn scan [s]
  (loop [s s
         tokens []]
    (if (= 0 (count s)) tokens
      (cond
        ; unambiguious one-character lexemes
        (unichar (first s)) (recur
                              (subs s 1)
                              (conj tokens (unichar (first s))))
        ; possibly ambiguous two-character combo lexemes
        (and (> (count s) 1) (ambig-twochar (subs s 0 2)))
        (recur (subs s 2) (conj tokens (ambig-twochar (subs s 0 2))))
        ; possibly ambiguous one-character lexemes
        (ambig-unichar (first s))
        (recur (subs s 1) (conj tokens (ambig-unichar (first s))))
        ; the default case of not recognised!
        :else (recur (subs s 1) (conj tokens :unrecognised))))))
