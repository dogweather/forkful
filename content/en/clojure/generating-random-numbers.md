---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:01.382380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

In Clojure, generating random numbers involves utilizing various functions to produce unpredictable numerical values, a common necessity in tasks ranging from generating unique identifiers to fueling algorithms in simulations or games. Understanding how to efficiently generate random numbers enables programmers to add a dynamic and unpredictable element to their applications.

## How to:

Clojure provides a straightforward way to generate random numbers. Here are a few methods:

1. **Generating a Random Double Between 0 (Inclusive) and 1.0 (Exclusive)**

```clojure
(rand)
```

This will yield a random floating-point number. For example:

```clojure
0.7095283176120234
```

2. **Generating a Random Integer Within a Specified Range**

```clojure
(rand-int n)
```

This function generates a random integer between 0 (inclusive) and `n` (exclusive). For instance, to generate a number between 0 and 10:

```clojure
(rand-int 10)
```

Sample output might be:

```clojure
7
```

3. **Generating a collection of random numbers**

You can also generate a collection of random numbers by combining `rand`, `rand-int`, or other random functions with collection-generating functions like `repeatedly`. For instance, to generate five random numbers between 0 and 100:

```clojure
(repeatedly 5 #(rand-int 100))
```

This might produce:

```clojure
[29 83 67 45 2]
```

## Deep Dive

Random number generation in Clojure is primarily achieved through Java interoperability, utilizing the `java.util.Random` class, reflecting a common approach in JVM-based languages. This implementation ensures a degree of consistency and reliability across JVM languages but may not offer the best performance or most features compared to specialized libraries.

For applications requiring more advanced random number generation features, such as generating numbers with specific distributions, Clojure can interop with dedicated libraries (e.g., Apache Commons Math for Java). Additionally, for cryptographic purposes, where predictability must be minimized, using `java.security.SecureRandom` is advisable over the standard Clojure or Java mechanisms.

Historically, random number generation has evolved significantly, from simple algorithms like the Linear Congruential Generator to more sophisticated ones used today. Clojure's approach, leveraging the underlying JVM capabilities, provides a balance between ease of use and the need for more specialized features. However, when true randomness or more control over the statistical properties of the generated numbers is required, exploring external libraries may provide better alternatives.

## See also

### Official Clojure Documentation
- [Clojure - `rand` function](https://clojuredocs.org/clojure.core/rand)

### Tutorials and Guides
- **Practical.li**: [Random number generation in Clojure](https://practical.li/clojure/numbers/random-numbers.html)
- **Clojure by Example**: [Generating Random Numbers](https://clojurebyexample.com/examples/random_numbers)
