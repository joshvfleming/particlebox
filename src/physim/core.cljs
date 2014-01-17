(ns physim.core
  (:require [THREE :as THREE]))

(def particle-count 30)
(def gravity-multiplier 100)
(def velocity-limit 10)

(def particles (atom []))
(def particle-meshes (atom []))

(defn rand-x
  []
  (- (rand-int window/innerWidth)
     (/ window/innerWidth 2)))

(defn rand-y
  []
  (- (rand-int window/innerHeight)
     (/ window/innerHeight 2)))

(defn generate-particles
  [n]
  (for [i (range n)]
    { :pos [(rand-x) (rand-y) (rand-y)]
      :vel [(/ (* 0 (rand-x)) window/innerWidth)
            (/ (* 0 (rand-y)) window/innerWidth)
            (/ (* 0 (rand-y)) window/innerWidth)] }))

(defn set-position!
  [mesh [x y z]]
  (set! (.-x (.-position mesh)) x)
  (set! (.-y (.-position mesh)) y)
  (set! (.-z (.-position mesh)) z))

(defn mesh-from-particle
  [p]
  (let [geometry (THREE.SphereGeometry. 20 20 20)
        material (THREE.MeshNormalMaterial.)
        mesh (THREE.Mesh. geometry material)]
    (set-position! mesh (:pos p))
    mesh))

(defn init-particles!
  [ps]
  (reset! particles ps)
  (reset! particle-meshes (map mesh-from-particle ps)))

(defn make-renderer!
  [meshes]
  (let [camera (THREE.PerspectiveCamera.
                50 (/ window/innerWidth
                      window/innerHeight) 1 10000)
        scene (THREE.Scene.)
        renderer (THREE.CanvasRenderer.)]
    (set! (.-z (.-position camera)) 1000)
    (doseq [m meshes]
      (.add scene m))
    (.setSize renderer window/innerWidth window/innerHeight)
    (.appendChild (.-body js/document) (.-domElement renderer))

    (fn []
      (.render renderer scene camera))))

(defn magnitude
  [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn calc-accel
  [p all]
  (reduce (fn [a n]
            (let [dir (map - (:pos n) p)
                  mag (magnitude dir)
                  dir (map #(/ % mag) dir)]
              (map + a
                   (if (> mag 0)
                     (map #(* % (/ gravity-multiplier (* mag mag))) dir)
                     [0 0 0]))))
          [0 0 0]
          all))

(defn move
  [p v all]
  (let [a (calc-accel p all)
        v (map + v a)
        magv (magnitude v)
        v (if (> magv velocity-limit)
            (map #(* (/ % magv) vel-limit) v)
            v)
        p (map + p v)]
    [p v a]))

(defn collided?
  [a b]
  (< (magnitude (map - a b)) 20))

(defn handle-collisions!
  []
  (let [n (count @particles)]
    (doseq [a (range n)
            b (range n)]
      (let [pa (nth @particles a)
            pb (nth @particles b)
            v (map + (:vel pa) (:vel pb))
            pa {:pos (:pos pa) :vel v}
            pb {:pos (:pos pb) :vel v}]
        (when (and (not= a b) (collided? (:pos pa) (:pos pb)))
          (do
            (reset! particles (assoc @particles a pa))
            (reset! particles (assoc @particles b pb))))))))

(defn motion-tick!
  []
  (doseq [i (range (count @particles))]
    (let [p (nth @particles i)
          [p v a] (move (:pos p) (:vel p) @particles)]
      (reset! particles
              (assoc (vec @particles) i {:pos p :vel v})))))

(init-particles! (generate-particles particle-count))
(def render-scene (make-renderer! @particle-meshes))  
(defn render
  []
  (motion-tick!)
  ;(handle-collisions!)

  (doseq [i (range (count @particles))]
    (let [p (nth @particles i)
          m (nth @particle-meshes i)]
      (set-position! m (:pos p))))

  (render-scene))
 
(defn animate
  []
  (.requestAnimationFrame js/window animate)
  (render))

(animate)
