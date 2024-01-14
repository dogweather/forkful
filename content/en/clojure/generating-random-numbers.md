---
title:    "Clojure recipe: Generating random numbers"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why


Generating random numbers is a common task in programming, especially in applications that require unpredictable or unknown values. In Clojure, there are multiple ways to generate random numbers, each with its own set of advantages and use cases.

## How To

To generate random numbers in Clojure, we can use the `rand` function from the `clojure.core` library. This function takes in one parameter, `n`, which represents the upper bound for the range of numbers to be generated. Let's take a look at an example:

```Clojure
; Generate a random number between 0 and 100
(rand 100)
```

This will output a decimal number between 0 and 100. If we want to get a whole number, we can use the `rand-int` function instead. Let's try this with a `for` loop to generate a sequence of 10 random numbers:

```Clojure
; Generate a sequence of 10 random numbers between 0 and 100
(for [n (range 10)]
  (rand-int 100))

; Output: (44 60 91 8 78 11 93 41 29 2)
```

We can also use `rand-nth` to get a random element from a collection. Here's an example using the `vector` data structure:

```Clojure
; Get a random element from a vector
(let [my-vec ["apple" "orange" "banana" "pineapple" "mango"]]
  (rand-nth my-vec))

; Output: "orange"
```

## Deep Dive

The `rand` function in Clojure uses the `java.util.Random` class behind the scenes, which is a popular pseudo-random number generator. It uses a "seed" value to generate a sequence of numbers that may appear random, but are actually deterministic based on the seed.

It's important to note that the `rand` function generates numbers between 0 (inclusive) and the specified upper bound (exclusive). This means that if we want a range of numbers that includes the upper bound, we need to add 1 to our upper bound. For example, if we want to generate a number between 1 and 10, we would use `(rand 11)`.

To generate truly random numbers, we can use the `secure-random` function from the `clojure.math.numeric-tower` library. This function uses a cryptographic random number generator, which is more secure for applications that deal with sensitive data.

## See Also

If you want to learn more about working with random numbers in Clojure, check out these helpful resources:

- [Official Clojure documentation on random numbers](https://clojure.org/reference/java_interop#_random_numbers)
- [ClojureDocs page on rand function](https://clojuredocs.org/clojure.core/rand)
- [Clojure for the Brave and True book chapter on random numbers](https://www.braveclojure.com/core-functions-in-depth/#random-numbers)
- [Clojure math.numeric-tower library](https://clojuredocs.org/clojure.math.numeric-tower/secure-random)