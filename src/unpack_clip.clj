(ns unpack-clip
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as S]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :as sh]))

(def cli-options
  ;; An option with a required argument
  [["-H" "--help"]
   ["-i" "--in dir" "Input images directory"
    :validate [(fn [arg]
                 (let [dir (io/file arg)]
                   (and (.exists dir) (.isDirectory dir)))) "Directorio debe existir"]]
   ["-o" "--out dir" "Output images directory"
    :validate [(fn [arg]
                 (let [dir (io/file arg)]
                   (or (not (.exists dir))
                       (.isDirectory dir)))) "Directorio no debe existir o ser un directorio"]]
   ])

(defn move-big [src-dir dest-dir]
  (let [files (filter (fn [f]
                        (and (.getName f) (re-matches #".*\.jpg$" (.getName f)) (> (.length f) 20000)))
                      (.listFiles src-dir))]
    (doseq [f files]
      (let [dest-file (io/file dest-dir (.getName f))]
        (.renameTo f dest-file)))))

(defn unpack-it [in out]
  (let [in (io/file in)
        out (doto (io/file out) (.mkdirs))
        tgzs (filter
              (fn [tgz]
                (let [name (.getName tgz)
                      [_ name ext] (re-matches #"(.*)(\.tgz)$" name)]
                  (and name (not (.exists (io/file out name))))))
              (.listFiles in))]
    (doseq [tgz tgzs]
      (let [name (.getName tgz)
            [_ name ext] (re-matches #"(.*)(\.tgz)$" name)
            src-dir (io/file out name)
            full-dir (doto (io/file out (str name "-full")) (.mkdirs))]
        (sh/with-sh-dir out
                        (println "expanding: " name)
                        (sh/sh "tar" "xzf" (.getCanonicalPath tgz))
                        (move-big src-dir full-dir))))))


(defn -main [& args]
  (println "Iniciando proceso de desempacado")
  (try
    (let [{:keys [options arguments errors summary] :as m} (parse-opts args cli-options)]
      (when (seq errors)
        (doseq [err errors]
          (println err))
        (System/exit 1))
      (let [{:keys [in out]} options]
        (println "Options:")
        (pp/pprint (into (sorted-map) options))
        (if (:help options)
          (println (:summary m))
          (if (and in out)
            (unpack-it in out)
            (println "Tanto -i -o (in y out) tienen que estar definidos")))))
    (System/exit 0)
  (catch Throwable e
    (println "Process terminated with error " e)
    (System/exit 1))))
