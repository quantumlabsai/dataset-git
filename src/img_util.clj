(ns img-util
  (:require
   [clojure.java.io :as io])
  (:import
   (java.awt.image BufferedImage)
   (javax.imageio ImageIO)))

(defn image-read [path]
  (ImageIO/read (io/file path)))

(defn image-write [img path & {:keys [fmt] :or {fmt "jpg"}}]
  (ImageIO/write img fmt (io/file path)))

(defn image-shape [img]
  [(.getWidth img) (.getHeight img) 3])

(defn image-type [img]
  (let [t (.getType img)]
    (if (= t 0)
      BufferedImage/TYPE_INT_ARGB
      t)))

(defn image-resize [img w h]
  (let [resized (BufferedImage. w h (image-type img))
        g (.createGraphics resized)]
    (.drawImage g img 0 0 w h nil)
    (.dispose g)
    resized))

(defn image-clip [img x y w h]
  (.getSubimage img x y w h))

;(def path "/Users/felipe/Projects/Python/pocs/placas/data/zacatecas-DS/Placas-train2/train.000001.jpg")

;(def img (image-read path))

;(println (image-shape img))

;(def img2 (image-resize img 300 480))

;(image-write img2 "cosa.jpg")
;(image-write (image-clip img 1330 800 120 65) "cosa2.jpg")
