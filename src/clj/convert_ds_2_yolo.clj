(ns convert-ds-2-yolo
  (:require [clojure.java.io :as io]
            [clojure.string :as S]))

;(def ROOT (io/file "/Users/felipedejesusgerard/tmp/linde/DATASETS/CIL-crit01.1-2022-08-10-12-e2637ac"))

;(def labels-train-file (io/file ROOT "labels-train.csv"))
;(def labels-val-file (io/file ROOT "labels-val.csv"))

;(def labels-train-lines  (-> labels-train-file
;                             (io/reader)
;                             (line-seq)))

;(def labels-val-lines  (-> labels-val-file
;                           (io/reader)
;                           (line-seq)))

(defn parse-line [line]
  (let [V (S/split line #",")
        file (first V)
        file (subs file 0 (- (count file) 4))
        [xmin xmax ymin ymax cls] (mapv #(Float/parseFloat %) (rest V))
        cls (dec (int cls))
        w  (/ (- xmax xmin) 480.0)
        h  (/ (- ymax ymin) 270.0)
        cx (/ (/ (+ xmax xmin) 2.0) 480.0)
        cy (/ (/ (+ ymax ymin) 2.0) 270.0)] 
    [file [cls cx cy w h]]))

(defn lines2annon-map [lines]
  (->> lines
       (map parse-line)
       (filter (fn [[_ [cls & _]]]
                 (>= cls 0)))
       (reduce (fn [result [k v]]
                 (update result k conj v))
               {})))

;(def labels-train (lines2annon-map labels-train-lines))

;(first labels-train)

(defn generate-annon-files [labels-dir labels-map]
  (doseq [[f-name annons] labels-map]
    (with-open [out (io/writer (io/file labels-dir (str f-name ".txt")))]
      (doseq [[cls cx cy w h] annons]
        (.write out (format "%s %s %s %s %s\n" cls cx cy w h))))))

;(generate-annon-files (io/file (io/file ROOT "labels") "train") labels-train)

;;;;;;;;;;;;;;;;;

;(def labels-val (lines2annon-map labels-val-lines))

;(first labels-val)

;(generate-annon-files (io/file (io/file ROOT "labels") "val") labels-val)

(defn copy-images [in out labels]
  (doseq [[f-name _] labels]
    (let [jpg-name (format "%s.jpg" f-name)
          in-file (io/file in jpg-name)
          out-file (io/file out jpg-name)]
      (println (format "%s -> %s" (.getCanonicalPath in-file) (.getCanonicalPath out-file)))
      (io/copy in-file out-file))))

(defn generate-yolo-ds-type [in out typ labels]
  (let [images-typ-file (io/file out (format "images/%s" typ))
        labels-typ-file (io/file out (format "labels/%s" typ))
        ]
    (if (and 
         (.mkdirs images-typ-file)
         (.mkdirs labels-typ-file))
      (do 
        (copy-images in images-typ-file labels)
        (generate-annon-files labels-typ-file labels)
        "Done")
      "Error cant create out directories")))

(defn create-yolo-ds-typ [in out typ]
  (let [labels-file (io/file in (format "labels-%s.csv" typ))
        labels-lines (-> labels-file
                         (io/reader)
                         (line-seq))
        labels (lines2annon-map labels-lines)]
    (generate-yolo-ds-type in out typ labels)))

(defn create-yolo-ds [in out]
  (format "train: %s, val: %s" 
          (create-yolo-ds-typ in out "train")
          (create-yolo-ds-typ in out "val")))

(defn -main [ds-in ds-out]
  (let [dir-in (io/file ds-in)
        dir-out (io/file ds-out)]
    (if (and 
         (.exists dir-in)
         (.isDirectory dir-in)
         (not (.exists dir-out)))
      (println (create-yolo-ds dir-in dir-out))
      (println (format "%s debe existir y ser un directorio y %s no deve existir, será creado" ds-in ds-out)))))





