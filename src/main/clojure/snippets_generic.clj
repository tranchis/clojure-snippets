(ns snippets-generic
  (:import [java.io
            BufferedWriter
            BufferedReader
            OutputStreamWriter
            InputStreamReader
            FileOutputStream
            FileInputStream]
           [java.util.zip
            GZIPOutputStream
            GZIPInputStream]))

(defn load-props
  "Receives a path and loads the Java properties for the file represented by the path inside the classpath (typically, a resource)."
  [file-name]
  (with-open [^java.io.Reader reader (clojure.java.io/reader (clojure.java.io/resource file-name))] 
    (let [props (java.util.Properties.)]
      (.load props reader)
      (into {} (for [[k v] props] [(keyword k) (read-string v)])))))

(defn nextelt
  "Given two characters, the previous row, and a row we are
  building, determine out the next element for this row."
  [char1 char2 prevrow thisrow position]
  (if (= char1 char2)
    (prevrow (- position 1))
    (+ 1 (min
           (prevrow (- position 1))
           (prevrow position)
           (last thisrow)))))
 
(defn nextrow
  "Based on the next character from string1 and the whole of string2
  calculate the next row. Initially thisrow contains one number."
  [char1 str2 prevrow thisrow]
  (let [char2 (first str2)
        position (count thisrow)]
    (if (= (count thisrow) (count prevrow))
      thisrow
      (recur
        char1
        (rest str2)
        prevrow
        (conj thisrow (nextelt char1 char2 prevrow thisrow position))))))
 
(defn levenshtein
  "Calculate the Levenshtein distance between two strings."
  ([str1 str2]
    (let [row0 (vec (map first (map vector (iterate inc 1) str2)))]
      (levenshtein 1 (vec (cons 0 row0)) str1 str2)))
  ([row-nr prevrow str1 str2]
    (let [next-row (nextrow (first str1) str2 prevrow (vector row-nr))
          str1-remainder (.substring str1 1)]
      (if (= "" str1-remainder)
        (last next-row)
        (recur (inc row-nr) next-row str1-remainder str2)))))


;;; Taken from https://gist.github.com/andrewvc/4334893

(defn gz-reader
  "Return a buffered reader for a gzipped file"
  [file-name]
  (BufferedReader.
   (InputStreamReader.
    (GZIPInputStream.
     (FileInputStream. file-name)))))
 
(defn gz-writer
  "Return a buffered writer for a gzipped file"
  [file-name]
  (BufferedWriter.
   (OutputStreamWriter.
    (GZIPOutputStream.
     (FileOutputStream. file-name)))))

