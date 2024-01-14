---
title:    "Clojure recipe: Generating random numbers"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a common task in programming. It allows developers to create unpredictable scenarios, simulate real-world randomness, and test code for robustness and edge cases.

## How To

To generate random numbers in Clojure, we can use the `rand` and `rand-int` functions. The `rand` function returns a floating-point number between 0 and 1, while the `rand-int` function returns an integer between 0 (inclusive) and the specified maximum value (exclusive).

```Clojure
(rand)        ; Outputs a random floating-point number 
              ; between 0 and 1, e.g. 0.4687318356
(rand 10)     ; Outputs a random integer between 0 and 9, e.g. 7
(rand-int 50) ; Outputs a random integer between 0 and 49, e.g. 23
```

We can also use `rand-nth` to select a random item from a sequence.

```Clojure
(def fruits ["apple" "orange" "banana" "grapes"])
(rand-nth fruits) ; Outputs a random fruit from the list, e.g. "banana"
```

To generate a sequence of multiple random numbers, we can use `repeatedly` and `take`.

```Clojure
(take 5 (repeatedly rand)) ; Outputs a sequence of 5 random floating-point numbers
                           ; between 0 and 1, e.g. (0.1245323 0.9426413 0.636262 0.390202 0.822322)
```

## Deep Dive

The `rand` and `rand-int` functions use the Java `java.lang.Math/random` method to generate random numbers. This method uses the current time in milliseconds as a seed, meaning that running the function multiple times without any changes in the program may result in the same values being generated. This can be avoided by using the `set!` function to set a specific seed before calling `rand` or `rand-int`.

```Clojure
; Sets the seed to 12345
(set! java.util.Random/seed 12345)
(rand) ; Outputs the same floating-point number every time, e.g. 0.6976311577
```

In addition, we can also use the `Random` class from the `java.util` package to generate random numbers with a specific seed or range.

```Clojure
(import java.util.Random) ; Import the Random class
(def random (Random.))     ; Create an instance of the Random class
(.nextInt random 10)      ; Outputs a random integer between 0 (inclusive) and 10 (exclusive), e.g. 7
```

## See Also

- [ClojureDocs: rand](https://clojuredocs.org/clojure.core/rand)
- [ClojureDocs: rand-int](https://clojuredocs.org/clojure.core/rand-int)
- [Oracle Java Documentation: java.util.Random](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)