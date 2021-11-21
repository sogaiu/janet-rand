# rand.janet
#
# A library of various functions for getting random things.
#
# Copyright 2021 John Gabriele

(defn rand-int
  ``
  Generate a random integer i, either in 0 <= i < n,
  or --- if `m` is given --- in n <= i < m.
  ``
  [n &opt m]
  (when m
    (assert (> m n)
            (string/format "m (%d) must be greater than n (%d)"
                           m n)))
  (def [start end]
    (if m
      [n m]
      [0 n]))
  (+ start
     (math/trunc (* (math/random)
                    (- end start)))))

(comment

  # XXX: docstring not so clear about this case?
  (rand-int 0)
  # => 0

  (rand-int 1)
  # => 0

  (try
    (rand-int 2 1)
    ([err]
      err))
  # => "m (1) must be greater than n (2)"

  (let [res (rand-int 2)]
    (and (not= 2 res)
         (or (zero? res)
             (one? res))))
  # => true

  (let [res (rand-int 1 3)]
    (and (not= 3 res)
         (or (one? res)
             (= 2 res))))
  # => true

  )

(defn rand-elem
  ``Choose a random element from array `arr`.``
  [arr]
  (get arr
       (rand-int (length arr))))

(comment

  (def animals
    @[:ant :bee :cat :dog :elephant :fox :giraffe :horse])

  (def animals-lookup
    (zipcoll animals
             (array/new-filled (length animals) true)))

  (get animals-lookup (rand-elem animals))
  # => true

  )

(defn rand-elems
  ``
  Randomly choose `n` elements from array `arr`. Any
  given element of `arr` may only be chosen once.
  ``
  [n arr]
  (assert (<= n (length arr))
          (string/format "n (%d) > length of array (%d)"
                         n (length arr)))
  (def res @[])
  (def ar2 (array ;arr))
  (for _ 0 n
    (let [i (rand-int (length ar2))]
      (array/push res (ar2 i))
      (array/remove ar2 i)))
  res)

(comment

  # XXX: tuple can be passed in too...
  (rand-elems 1 [:x])
  # => @[:x]

  (try
    (rand-elems 2 @[:breath])
    ([err]
      err))
  # => "n (2) > length of array (1)"

  (def animals
    @[:ant :bee :cat :dog :elephant :fox :giraffe :horse])

  (type (rand-elems 1 animals))
  # => :array

  (-> (rand-elems 1 animals)
      length
      one?)
  # => true

  (def animals-lookup
    (zipcoll animals
             (array/new-filled (length animals) true)))

  (get animals-lookup
       (first (rand-elems 1 animals)))
  # => true

  (let [n-friends (rand-int 1 (inc (length animals)))
        some-friends (rand-elems n-friends animals)]
    (def count
      (->> some-friends
           (map |(get animals-lookup $))
           length))
    (and (= n-friends
            count)
         (deep= (sort (distinct some-friends))
                (sort some-friends))))
  # => true

  )

(defn rand-rolls
  ``
  Like the rolls of a die, choose `n` values randomly
  from the elements in the array `arr`. A given value may
  be selected more than once.
  ``
  [n arr]
  (var res @[])
  (for _ 0 n
    (array/push res
                (rand-elem arr)))
  res)

(comment

  (def suits
    @[:club :diamond :heart :spade])

  (type suits)
  # => :array

  (-> (rand-rolls 1 suits)
      length
      one?)
  # => true

  (def suits-lookup
    (zipcoll suits
             (array/new-filled (length suits) true)))

  (get suits-lookup
       (first (rand-rolls 1 suits)))
  # => true

  (let [n-draws (rand-int 1 (inc (length suits)))
        some-draws (rand-rolls n-draws suits)]
    (def count
      (->> some-draws
           (map |(get suits-lookup $))
           length))
    (and (= n-draws
            count)
         (<= (length (distinct some-draws))
             n-draws)))
  # => true

  )

(defn shuffle
  ``Return a shuffled copy of `arr`.``
  [arr]
  (rand-elems (length arr) arr))

(comment

  # XXX: tuple can be passed in too
  (type
    (shuffle [:x :y :z]))
  # => :array

  (deep= (shuffle @[:smile])
         @[:smile])
  # => true

  (def actions
    @[:breathe :laugh :sit :sleep :smile :think :walk])

  (deep= actions
         (sort (shuffle actions)))
  # => true

  )

(defn rand-key
  ``Get a random key from table `t`.``
  [t]
  (rand-elem (keys t)))

(comment

  (def colors
    @{:blue 4
      :green 5
      :orange 6
      :red 3
      :yellow 6})

  (let [choice (rand-key colors)]
    (truthy? (get colors choice)))
  # => true

  )

(defn rand-val
  ``Get a random value from table `t`.``
  [t]
  (rand-elem (values t)))

(comment

  (def colors
    @{:blue 4
      :green 5
      :orange 6
      :red 3
      :yellow 6})

  (let [choice (rand-val colors)
        vals (values colors)]
    (and (int? choice)
         (<= (min ;vals)
             choice
             (max ;vals))))
  # => true

  )

(defn rand-kv
  ``
  Get a random key/value pair (as a tuple)
  from table `t`.
  ``
  [t]
  (rand-elem (pairs t)))

(comment

  (def some-pairs
    @{:left :right
      :light :dark
      :sun :moon
      :wind :water
      :wake :sleep})

  (let [[one another] (rand-kv some-pairs)]
    (truthy?
      (and (get some-pairs one)
           (get (invert some-pairs) another))))
  # => true

  )

(defn rand-str
  ``
  Generate a random lowercase ascii string,
  `n` characters long.
  ``
  [n]
  (def ltrs (map string/from-bytes
                 "abcdefghijklmnopqrstuvwxyz"))
  (string/join (rand-rolls n ltrs)))

(comment

  (let [str-len (rand-int 1 8)
        a-str (rand-str str-len)]
    (= (length (map |(<= (chr "a")
                         $
                         (chr "z"))
                    a-str))
       str-len))
  # => true

  )
