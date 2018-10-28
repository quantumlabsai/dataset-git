(ns ds-manager
  (:require [clojure.string :as S]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.java.shell :refer [sh]]))

(def levels {:debug 0
             :info  1
             :warn  2
             :error 3})

(def verbosity (atom :info))

(defn set-verbosity [level]
  (assert (levels level) "Niveles posibles :debug :info :warn :error")
  (reset! verbosity level))

(defn inform [level msg & {:keys [pprint] :or {pprint false}}]
  (if (>= (level levels) (@verbosity levels))
    (if pprint
      (pp/pprint msg)
      (println msg))))

(defn is-ds-dir? [dir]
  (let [annon-dir (io/file dir "annotations")
        with-jpg (some #(re-matches #".*\.jpg$" (.getName %)) (.listFiles dir))
        sub-dirs (count (filter #(.isDirectory %) (.listFiles dir)))
        with-annons (.exists annon-dir)
        is-leaf (or (= 0 sub-dirs) (and with-annons (= 1 sub-dirs)))
        it-is (and with-jpg is-leaf)]
    (inform :debug (format "dir: %s, with-jpg: %s, sub-dirs: %d, with-annons: %b, is-leaf: %b" (.getName dir) with-jpg sub-dirs with-annons is-leaf))
    (if it-is
      {:jpg (count (filter #(re-matches #".*\.jpg$" (.getName %)) (.listFiles dir)))
       :xml (if with-annons (count (filter #(re-matches #".*\.xml$" (.getName %)) (.listFiles annon-dir))) 0)})))

(defn create-ds-set [root-dir excluding-set ds-set total-jpg total-xml]
 (loop [[sub-dir & sub-dirs] (filter #(.isDirectory %) (.listFiles root-dir))
        ds-set ds-set
        total-jpg total-jpg
        total-xml total-xml]
   (if sub-dir
     (let [ds-dir-info (is-ds-dir? sub-dir)]
       (inform :debug [:sub-dir ds-dir-info total-jpg total-xml])
       (cond
         ds-dir-info
         (do
           (inform :debug (format ">>> sub-dir: %s, (excluding-set sub-dir): %s, (ds-set sub-dir): %s" sub-dir (excluding-set sub-dir) (ds-set sub-dir)))

           (if (or (excluding-set (.getName sub-dir)) (ds-set (.getName sub-dir)))
             (throw (Exception. (format "Directorios dentro del dataset deben ser únicos, problema con '%s'" (.getCanonicalPath sub-dir)))))
           (recur sub-dirs
                  (assoc ds-set (.getName sub-dir) ds-dir-info)
                  (+ total-jpg (:jpg ds-dir-info))
                  (+ total-xml (:xml ds-dir-info))))

         (.isDirectory sub-dir)
         (let [[ds-set total-jpg total-xml] (create-ds-set sub-dir excluding-set ds-set total-jpg total-xml)]
           (recur sub-dirs ds-set total-jpg total-xml))

         :OTHERWISE
         (recur sub-dirs ds-set total-jpg total-xml)))
     [ds-set total-jpg total-xml])))

(defn create-ds-info [root-dir-path]
  (let [root-dir (io/file root-dir-path)
        train-dir (io/file root-dir "train")
        val-dir (io/file root-dir "val")
        _ (inform :info "Scaning train")
        [train-set train-jpg train-xml] (create-ds-set train-dir {} {} 0 0)
        _ (inform :info "Scaning val")
        [val-set val-jpg val-xml] (create-ds-set val-dir train-set {} 0 0)]
    (inform :info "Done")
    (inform :debug train-set :pprint true)
    (inform :debug "----------------")
    (inform :debug val-set :pprint true)
    (let [result {:train (into (sorted-map) train-set)
                  :train-total {:images train-jpg :xml train-xml}
                  :val (into (sorted-map) val-set)
                  :val-total {:images val-jpg :xml val-xml}}]
      (inform :debug result :pprint true)
      result)))
