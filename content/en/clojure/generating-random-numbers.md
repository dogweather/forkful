---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:47.570320-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers are unpredictable figures used in programs for stuff like games, simulations, and security. Coders use them because they stir in surprise, mimic real-world chaos, or safeguard data.

## How to:

In Clojure, the `rand`, `rand-int`, and `rand-nth` functions are your go-to for randomness:

```Clojure
(rand) ; a random double between 0.0 and 1.0
=> 0.7095282210647944

(rand-int 10) ; a random integer between 0 (inclusive) and 10 (exclusive)
=> 7

(rand-nth [1 2 3 4 5]) ; a random element from the collection
=> 3
```

Tweak the seed for repeatable results:

```Clojure
(use 'clojure.java.io)
(binding [java.util.Random/*rnd* (java.util.Random. 42)]
  (rand-int 100))
=> 81
```

## Deep Dive

Clojure’s random functions piggyback on Java’s `java.util.Random` class, woven tightly with the JVM. Historically, generating truly random numbers on computers is tough, so "pseudorandom number generators" (PRNGs) are used, which are predictable if you know the starting point, called the "seed".

The `rand` function, without arguments, shells out a floating-point number between 0 and 1. It’s a typical use case for stuff like probabilistic algorithms or simulating random events in a bounded space.

`rand-int` and `rand-nth` are variations on a theme for when you need integers or you're picking from a list. `rand-int` is perfect for games where you might be rolling a virtual dice or choosing a random enemy to spawn.

Besides the core functions, there's java interop if you need heavier calibers. You could invoke `java.util.Random` directly or go nuclear with `SecureRandom` for cryptographic stuff. But remember, `SecureRandom` is overkill for most non-security uses and will slow down your program.

## See Also

- The Clojure cheatsheet on randomness: [https://clojure.org/api/cheatsheet](https://clojure.org/api/cheatsheet)
- Exploring Java’s Random: [https://docs.oracle.com/javase/8/docs/api/java/util/Random.html](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- When you need crypto-grade randomness: [https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- A deep-dive into randomness and Clojure's approach: ["Clojure from the ground up: modeling"](https://aphyr.com/posts/303-clojure-from-the-ground-up-modeling)