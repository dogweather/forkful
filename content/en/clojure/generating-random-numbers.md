---
title:                "Clojure recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a useful task in many programming applications. Whether you need to create randomized test data, simulate probabilistic events, or simply add an element of unpredictability to your program, understanding how to generate random numbers in Clojure can greatly enhance your coding capabilities.

## How To

To generate random numbers in Clojure, we can use the `rand` function from the `clojure.core` library. This function takes in a range of numbers and returns a random number within that range. Let's see an example of how this works:

```Clojure
(rand 10) ; returns a random number between 0 and 10
(rand 1 100) ; returns a random number between 1 and 100
(rand 5 20) ; returns a random number between 5 and 20
```

We can also use the `rand-nth` function to randomly select an element from a given sequence. For example:

```Clojure
(def fruits ["apple" "orange" "banana" "strawberry"])

(rand-nth fruits) ; returns a random fruit from the list
```

We can even generate a random string by first creating a list of characters and then using the `apply` function to concatenate them into a string:

```Clojure
(def letters ["a" "b" "c" "d" "e"])

(apply str (take 5 (repeat (rand-nth letters)))) ; returns a random string of 5 characters
```

## Deep Dive

The `rand` function uses the `java.util.Random` class, which is a pseudo-random number generator. This means that the numbers generated are not truly random, but are instead determined by a mathematical algorithm. To ensure a more diverse range of numbers, we can use the `clojure.math.random` library, which uses a different algorithm and provides a more evenly distributed range of random numbers.

Additionally, we can use the `seed` function to set a starting point for the random number generator. This can be useful for creating reproducible results or for creating a series of seemingly random but actually predetermined numbers.

## See Also

- [Official Clojure Documentation on Random Numbers](https://clojure.org/guides/math#random_numbers)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)