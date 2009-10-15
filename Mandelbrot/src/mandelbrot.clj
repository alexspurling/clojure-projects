(import '(javax.swing JFrame JPanel)
        '(java.awt Color Dimension Graphics)
        '(java.awt.image BufferedImage))

;; Maximum number of iterations of the mandelbrot formula to make
(def max-iters 50)

;; Applies the mandelbrot formula
(defn mandel [[xcoord ycoord]]
  [(- (* xcoord xcoord) (* ycoord ycoord)) (* 2 xcoord ycoord)])

(defn mandelset [xcoord ycoord]
  "Returns an infinite sequence of vectors containing the values of 
  successive iterations of the mandelbrot formula, given a point on
  the complex plane."
  (iterate
    #(vec (map + (mandel %) [xcoord ycoord]))
    [xcoord ycoord]))


;; Colours used to draw the set
(def grad-colour-a [255, 255, 0])
(def grad-colour-b [0, 0, 255])
(def set-colour [0, 0, 0])

(defn grad-colour
  "Returns the colour that is the given fraction of the way between
  the first and second colours given. Returns as a vector of three
  integers between 0 and 255."
  [col1 col2 frac]
  (vec (map #(+ (* frac (- %2 %1)) %1) col1 col2)))

(defn iter-colour 
  "Returns the colour needed to paint a point with the given number
  of iterations"
  [num-iters]
  (grad-colour grad-colour-a grad-colour-b
    (/ (double num-iters) max-iters)))

(defn mag [[x y]]
  (+ (* x x) (* y y)))

(defn coord-colour 
  "Returns a colour for which to draw the given coordinate. If the coordinate
  is within the mandelbrot set, black is returned. Otherwise, a colour within
  a gradient is given based on the number of iterations of the mandelbrot set
  that have been evaluated."
  [[xcoord ycoord]]
  (let [num-iters (count (take max-iters (take-while #(<= (mag %) 4) (mandelset xcoord ycoord))))]
    (if (= max-iters num-iters)
      set-colour
      (iter-colour num-iters))))


;; Size of the canvas in pixels
(def width 600)
(def height 500)

;; Coordinates of top left pixel in the complex plane
(def x-coord-start -2)
(def y-coord-start -1.25)

;; Size of the x and y dimensions in the complex plane
(def x-coord-size 3)
(def y-coord-size 2.5)

(defn get-coord 
  "Returns the coordinates of the given pixel in the complex plane"
  [x y]
  [(+ x-coord-start (* (/ x width) x-coord-size))
   (+ y-coord-start (* (/ y height) y-coord-size))])

(def pixel-colours
  (for [x (range (inc width)) y (range (inc height))]
    (coord-colour (get-coord x y))))

(def img (BufferedImage. width height (BufferedImage/TYPE_INT_RGB)))
(def wr (.getRaster img))

(defn draw-image [graphics]
  (.drawImage graphics img 0 0 (Color/red) nil))
  
(def panel (doto (proxy [JPanel] [] (paint [g] (draw-image g)))))

;; Returns a sequence of vectors representing the pixel coordinates
(def pixels
  (for [y (range height) x (range width)]
     [x y]))

(defn render[] 
  (time (dorun (pmap 
    (fn [pixel] 
      (let [[x y] pixel] 
        (.setPixel wr x y (int-array (coord-colour (get-coord (double x) (double y))))))) pixels)))
  (.repaint panel))

(let [frame (JFrame.)]
    (.setPreferredSize panel (Dimension. width height))
    (doto frame 
      (.add panel)
      .pack
      (.setLocationRelativeTo nil)
      .show))

(future (render))
