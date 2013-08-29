(ns snippets-generic)

(defn load-props
  [file-name]
  (with-open [^java.io.Reader reader (clojure.java.io/reader (clojure.java.io/resource file-name))] 
    (let [props (java.util.Properties.)]
      (.load props reader)
      (into {} (for [[k v] props] [(keyword k) (read-string v)])))))
