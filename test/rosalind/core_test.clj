(ns rosalind.core-test
  (:require [clojure.test :refer :all]
            [rosalind.core :refer :all]))

(deftest dna-test
  (testing "Counting DNA Nucleotides"
    (is (= (dna "")
           '(0 0 0 0)))
    (is (= (dna "A")
           '(1 0 0 0)))
    (is (= (dna "C")
           '(0 1 0 0)))
    (is (= (dna "G")
           '(0 0 1 0)))
    (is (= (dna "T")
           '(0 0 0 1)))
    (is (= (dna "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
           '(20 12 17 21)))))


(deftest rna-test
  (testing "Transcribing DNA into RNA"
    (is (= (rna "") ""))
    (is (= (rna "A") "A"))
    (is (= (rna "C") "C"))
    (is (= (rna "G") "G"))
    (is (= (rna "T") "U"))
    (is (= (rna "GATGGAACTTGACTACGTAAATT") "GAUGGAACUUGACUACGUAAAUU"))))


(deftest revc-test
  (testing "Complementing a Strand of DNA"
    (is (= (revc "") ""))
    (is (= (revc "A") "T"))
    (is (= (revc "C") "G"))
    (is (= (revc "G") "C"))
    (is (= (revc "T") "A"))
    (is (= (revc "AAAACCCGGT") "ACCGGGTTTT"))
    (is (= (revc "ACCGGGTTTT") "AAAACCCGGT"))
    (is (= (revc "CAGT") "ACTG"))
    (is (= (revc "ACTG") "CAGT"))))

(deftest hamm-test
  (testing "Counting Point Mutations"
    (is (= (hamm "" "") 0))
    (is (= (hamm "AA" "AA") 0))
    (is (= (hamm "CC" "CC") 0))
    (is (= (hamm "ACTG" "ACTG") 0))
    (is (= (hamm "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT") 7))))


(deftest prot-test
  (testing "Counting Point Mutations"
    (is (= (prot "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
           "MAMAPRTEINSTRING"))))

(deftest gc-content-test
  (testing "Computing GC-content of a DNA string"
    (is (= (gc-content "A") 0.0))
    (is (= (gc-content "C") 100.0))
    (is (= (gc-content "T") 0.0))
    (is (= (gc-content "G") 100.0))
    (is (= (gc-content "ACTG") 50.0))
    (is (= (gc-content "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT")
           60.91954022988506))))

(def fasta-test-string
  ">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT
")

(deftest parse-fasta-test
  (testing "Parsing of a FASTA string"
    (is (= (parse-fasta ">a\nA\n>c\nC")
           {"a" "A", "c" "C"}))
    (is (= (parse-fasta fasta-test-string)
           {"Rosalind_6404" "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG",
            "Rosalind_5959" "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC",
            "Rosalind_0808" "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"}))))


(deftest gc-content-test
  (testing "Finding DNA string with maximun gc-contet in a FASTA-formatted string"
    (is (= (gc fasta-test-string)
           ["Rosalind_0808" 60.91954022988506]))))
