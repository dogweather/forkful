---
date: 2024-01-25 03:00:40.655499-07:00
description: 'How to: In Clojure, we primarily use `Math/round`, `Math/floor`, and
  `Math/ceil`.'
lastmod: '2024-03-13T22:44:59.740642-06:00'
model: gpt-4-1106-preview
summary: In Clojure, we primarily use `Math/round`, `Math/floor`, and `Math/ceil`.
title: Rounding numbers
weight: 13
---

## How to:
In Clojure, we primarily use `Math/round`, `Math/floor`, and `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

For specific decimal places, we multiply, round, and divide:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Deep Dive
Before fancy programming languages, rounding was a manual process, think abacus or paper. In programming, it's crucial for number representation due to floating-point precision limitations. 

Alternatives for rounding include using the `BigDecimal` class for precision control or libraries like `clojure.math.numeric-tower` for advanced math functions. Clojure's `Math/round` relies on Java's `Math.round`, `Math/floor`, and `Math/ceil` functions, which means it inherits the same float and double nuances.

Implementation-wise, when rounding in Clojure, remember it automatically uses double precision when dealing with decimals. Careful with rounding errors!

## See Also
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Understanding Floating-Point Precision: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
