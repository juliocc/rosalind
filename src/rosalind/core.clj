(ns rosalind.core)

(defn dna [s]
  (map (frequencies s) [\A \C \G \T]))
