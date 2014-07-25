(ns rosalind.core)

(defn get-line [n filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (nth (line-seq rdr) n)))

(defn dna [s]
  (map #((frequencies s) %1 0) [\A \C \G \T]))

(defn rna [s]
  (clojure.string/replace s #"T" "U"))

(defn revc [s]
  (let [complements {\A \T, \T \A, \C \G, \G \C}]
    (apply str (map complements (reverse s)))))

(defn hamm [a b]
  (apply + (map #(if (= %1 %2) 0 1) a b)))

(defn prot [s]
  (let [codons {
                "UUU" "F",      "CUU" "L",      "AUU" "I",      "GUU" "V",
                "UUC" "F",      "CUC" "L",      "AUC" "I",      "GUC" "V",
                "UUA" "L",      "CUA" "L",      "AUA" "I",      "GUA" "V",
                "UUG" "L",      "CUG" "L",      "AUG" "M",      "GUG" "V",
                "UCU" "S",      "CCU" "P",      "ACU" "T",      "GCU" "A",
                "UCC" "S",      "CCC" "P",      "ACC" "T",      "GCC" "A",
                "UCA" "S",      "CCA" "P",      "ACA" "T",      "GCA" "A",
                "UCG" "S",      "CCG" "P",      "ACG" "T",      "GCG" "A",
                "UAU" "Y",      "CAU" "H",      "AAU" "N",      "GAU" "D",
                "UAC" "Y",      "CAC" "H",      "AAC" "N",      "GAC" "D",
                "UAA" "",       "CAA" "Q",      "AAA" "K",      "GAA" "E",
                "UAG" "",       "CAG" "Q",      "AAG" "K",      "GAG" "E",
                "UGU" "C",      "CGU" "R",      "AGU" "S",      "GGU" "G",
                "UGC" "C",      "CGC" "R",      "AGC" "S",      "GGC" "G",
                "UGA" "",       "CGA" "R",      "AGA" "R",      "GGA" "G",
                "UGG" "W",      "CGG" "R",      "AGG" "R",      "GGG" "G",
                }]
    (apply str (map #(codons (apply str %1)) (partition 3 s)))))


(defn gc-content [s]
  (* 100  (/
           (+ (count (filter #(= \C %1) s))
              (count (filter #(= \G %1) s)))
           (double (count s)))))


(defn parse-fasta [s]
  (into {}
        (for [match (re-seq #">(.*)([\nCTGA]*)" s)]
          [(nth match 1)
           (apply str
                  (remove #(= \newline %1)
                          (nth match 2)))])))

(defn gc [s]
  (first (sort-by second
                  #(compare %2 %1)
                  (map (fn [[label s]]
                         [label (gc-content s)])
                       (parse-fasta s)))))
