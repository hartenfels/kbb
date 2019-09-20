(ns kbb.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.nio.file LinkOption Files Path Paths])
  (:gen-class))


(defn empty-array [class]
  (into-array class []))

(defn stream-seq [stream]
  (-> stream .iterator iterator-seq))


(defn path [path-or-string & more]
  (if (instance? Path path-or-string)
    (if (empty? more)
      path-or-string
      (reduce #(.resolve %1 %2) path-or-string more))
    (apply path (cons (Paths/get path-or-string (empty-array String)) more))))

(defmacro is-x? [predicate path-or-string]
  `(~predicate (path ~path-or-string) (empty-array LinkOption)))

(defn is-directory? [path-or-string]
  (is-x? Files/isDirectory path-or-string))

(defn is-file? [path-or-string]
  (is-x? Files/isRegularFile path-or-string))

(defn basename [path-or-string]
  (str (.getFileName (path path-or-string))))

(defn ls [dir]
  (stream-seq (Files/list (path dir))))

(defn read-first-non-blank-line [path-or-string]
  (with-open [reader (Files/newBufferedReader (path path-or-string))]
    (->> reader
         line-seq
         (filter (complement string/blank?))
         first)))


(defn- list-with-predicate [dir predicate]
  (->> dir
       ls
       (filter predicate)
       (sort-by basename)))

(defn list-columns [dir]
  (list-with-predicate dir #(and (is-directory? %)
                                 (re-find #"\A[0-9]+.*\S" (basename %)))))

(defn list-tasks-in-column [column]
  (list-with-predicate column is-file?))


(defn to-column-and-task-map [column]
  {:column column
   :tasks (into [] (list-tasks-in-column column))})

(defn read-all-tasks-by-columns [dir]
  (->> dir
       list-columns
       (map to-column-and-task-map)))


(defn- strip-leading-digits [column]
  (string/replace-first column #"\A[0-9]+" ""))

(defn column-title [column]
  (-> column basename strip-leading-digits string/trim))

(defn task-title [task]
  (or (read-first-non-blank-line task)
      (-> task basename string/trim)))


(defn- tasks-titles-by-column-title [{:keys [column tasks]}]
  {:column (column-title column)
   :tasks (map task-title tasks)})

(defn all-task-titles-by-column-titles [dir]
  (map tasks-titles-by-column-title (read-all-tasks-by-columns dir)))


(defn format-tasks-by-column [{:keys [column tasks]}]
  (concat [column]
          [(string/replace column #"." "=")]
          [""]
          (map (partial str "* ") tasks)
          (if (empty? tasks) [] [""])))

(defn format-all-tasks-by-columns [dir]
  (string/join "\n" (->> dir
                         all-task-titles-by-column-titles
                         (mapcat format-tasks-by-column))))


(defn -main []
  (let [dir (or (System/getenv "KBB_ROOT")
                (path (System/getenv "HOME") ".kbb-board"))]
    (println (format-all-tasks-by-columns dir))))
