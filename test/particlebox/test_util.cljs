(ns particlebox.test-util)

(defn roughly=
  ([a b] (roughly= a b 0.01))
  ([a b tolerance]
     (if (and (coll? a) (coll? b))
       (every? identity (map roughly= a b))
       (< (Math/abs (- a b)) tolerance))))

