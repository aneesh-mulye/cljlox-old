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

(def whitespace #{\space \tab \return})

(def reserved-words
  {"and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "for"    :for
   "fun"    :fun
   "fi"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while})

;; TODO: add error handling! Return a map with parsed tokens,
;; and encountered errors with line numbers, rather than the current
;; assumption of working only on valid code.
(defn scan [s]
  (loop [linenum 1
         s s
         tokens []]
    (if (= 0 (count s)) tokens
      (cond
        ; unambiguious one-character lexemes
        (unichar (first s)) (recur
                              linenum
                              (subs s 1)
                              (conj tokens (unichar (first s))))
        ; possibly ambiguous two-character combo lexemes
        (and (> (count s) 1) (ambig-twochar (subs s 0 2)))
        (recur
          linenum
          (subs s 2)
          (conj tokens (ambig-twochar (subs s 0 2))))
        ; possibly ambiguous one-character lexemes
        (ambig-unichar (first s))
        (recur linenum (subs s 1) (conj tokens (ambig-unichar (first s))))
        ; whitespace
        (whitespace (first s)) (recur linenum (subs s 1) tokens)
        ; strings. Damn, let's see.
        (re-find #"^\"([\s\S]*?)\"" s)
        (let [[fullstring strval] (re-find #"^\"([\s\S]*?)\"" s)
              skip (count fullstring)]
          (recur
            (+ linenum
               (count (filter #(= \newline %) strval)))
            (subs s skip)
            (conj tokens [:string strval])))
        ; unterminated string
        (= \" (first s))
        (recur linenum (subs s 1) (conj tokens :open-unterminated-string))
        ; newline
        (= \newline (first s))
        (recur (inc linenum) (subs s 1) tokens)
        ; number literals
        (re-find #"^\d+\.\d+|^\d+" s)
        (let [numlit (re-find #"^\d+\.\d+|^\d+" s)
              skip (count numlit)
              numval (parse-double numlit)]
          (recur
            linenum
            (subs s skip)
            (conj tokens [:number numval])))
        ; identifiers, subcase: reserved words
        (re-find #"^[a-zA-Z_]\w+" s)
        (let [ident (re-find #"^[a-zA-Z_]\w+" s)
              skip (count ident)]
          (recur
            linenum
            (subs s skip)
            (conj tokens
                  (or (reserved-words ident) :identifier))))
        ; the default case of not recognised!
        :else (recur linenum (subs s 1) (conj tokens :unrecognised))))))
