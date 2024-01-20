---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Programming isn't just fixed variables and predictable outcomes. There're times where randomness plays a key role. Whether it's for game development, data analysis, or encryption, generating random numbers keeps things versatile, fun, and secure.

## How to:
Let's head in. Random number generation in Clojure is no hard task. 

To generate a simple random integer, you can do:

```clojure
(let [rand-int (rand-int 100)] 
  (println "Random integer:" rand-int))
```
This will output a random integer between 0 and 99. You can, of course, replace 100 with any number to change the range.

Similarly for a random floating-point number:

```clojure
(let [rand-num (rand)] 
  (println "Random number:" rand-num))
```
This will generate a floating-point number between 0 (inclusive) and 1 (exclusive).

## Deep Dive
Now, let's dive deeper. 

1. **Historical Context**: Randomness has played a pivotal role in programming since its inception. In the early days, pseudo-random number generators like the middle-square method were used. But, with improved technology, we now have in-built functions to generate random numbers like we just saw in Clojure.

2. **Alternatives**: Clojure accepts seeds which can be useful when you want to reproduce the same sequence of "random" numbers. This is particularly handy in testing.

```clojure
(java.util.Random. 0) ;; Seed 0
```
3. **Implementation Details**: When you call `(rand)`, Clojure sources environment noise like the current timestamp to produce randomness. For `(rand-int n)`, it scales the `(rand)` output to the desired range.

## See Also
Make sure you explore further!

- Official Clojure Documentation: [Random Numbers](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand)
  

- Testing with seed: [StackOverflow Thread](https://stackoverflow.com/questions/25609153/how-to-return-random-but-deterministic-results-in-clojure)