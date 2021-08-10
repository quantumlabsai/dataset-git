(ns saca-vehiculo-grande-sin-placa
  (:require [clojure.xml :as X]
            [clojure.zip :as Z]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))


(def files (->> (io/file "/Users/felipe/tmp/2021-08-09-11/annotations")
                (.listFiles)
                (filter #(re-matches #".*xml$" (.getName %)))))

(defn filter-obj-cls [cls-set obj-loc]
  (let [z (Z/xml-zip obj-loc)]
    (and (= :object (:tag obj-loc))
         (cls-set (->> z Z/down Z/down Z/node)))))

(defn node2vec [n]
  (let [z (Z/xml-zip n)
        cls (->> z Z/down Z/down Z/node Integer/parseInt)
        bbox (mapv (fn [coord-node]
                     (Integer/parseInt (first (:content coord-node)))) (->> z Z/down Z/rightmost Z/children))]
    (into [cls] bbox)))

(defn big-vehicle-no-plate [f]
  (let [xml (X/parse f)
        zip (Z/xml-zip xml)
        objects (->> (Z/children zip)
                     (filter (partial filter-obj-cls #{"1" "2" "5"}))
                     (mapv node2vec))]
    (println :-> (count objects) (first objects))
    (when (and (seq objects)
               (= 1 (count objects))
               (= 1 (ffirst objects))
               (> (- ((first objects) 3) ((first objects) 1)) 200))
      f)))

(defn copy-it [out-dir out-annon xml-file-in]
  (let [xml-name (.getName xml-file-in)
        jpg-name (str (subs xml-name 0 (- (count xml-name) 3)) "jpg")
        jpg-file-in (io/file (.getParentFile (.getParentFile xml-file-in)) jpg-name)
        jpg-file-out (io/file out-dir jpg-name)
        xml-file-out (io/file out-annon xml-name)]
    (println :copying (.getAbsolutePath xml-file-in) (.getAbsolutePath xml-file-out))
    (io/make-parents xml-file-out)
    (io/copy xml-file-in xml-file-out)
    (io/copy jpg-file-in jpg-file-out)))

(defn extract-big-vehicle-no-plate-dataset [root-path pattern out-path]
  (let [out (io/file out-path)
        out-annon (io/file out "annotations")
        root (io/file root-path)
        pattern (re-pattern pattern)
        subdirs (filterv #(and 
                           (.isDirectory %)
                           (re-matches pattern (.getName %)))
                         (.listFiles root))]
    (doseq [subdir subdirs]
      (println :subdir subdir)
      (let [annon (io/file subdir "annotations")
            xmls (filter #(and (.isFile %)
                               (re-matches #".*xml$" (.getName %)))
                         (.listFiles annon))]
        (println :xmls xmls)
        (doseq [xml-file xmls]
          (if (big-vehicle-no-plate xml-file)
            (copy-it out out-annon xml-file)
            (println :skiping (.getName xml-file))))))))

(extract-big-vehicle-no-plate-dataset 
 "/Users/felipe/tmp/TEST-BIG-VEHICLE" "2021.*" 
 "/Users/felipe/tmp/TTT")


