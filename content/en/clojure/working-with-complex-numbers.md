---
title:                "Working with complex numbers"
aliases:
- en/clojure/working-with-complex-numbers.md
date:                  2024-01-25T03:00:07.667826-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers extend the real numbers with an additional part, the imaginary unit 'i'. Programmers use them in various domains, including signal processing, electromagnetic theory, and fractals, where calculations involving the square root of a negative number are routine.

## How to:
Clojure provides built-in support for complex numbers through the `clojure.lang.Numbers` utility class. Use `complex` to create complex numbers and perform arithmetic.

```clojure
;; Creating complex numbers
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Addition
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Subtraction
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Multiplication
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Division
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Conjugate
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Deep Dive
Complex numbers were formalized by mathematicians like Gauss and Euler in the 18th century. Though initially met with skepticism, they've since become crucial in modern science and engineering. Clojure doesn’t have a native complex number type like some languages (e.g., Python), but the included Java interop can handle the necessary operations via the `clojure.lang.Numbers` class.

Java's `java.lang.Complex` is a robust alternative, providing more features and potential optimizations. Clojure’s host interoperability makes it easy to work with Java libraries.

Under the hood, complex number arithmetic involves adding and multiplying the real and imaginary parts, with the key rule that `i^2 = -1`. Complex number division can be more convoluted, typically requiring the conjugate to avoid division by complex numbers.

## See Also
- The ClojureDocs, for a quick reference: https://clojuredocs.org/
- The Java API for `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- The Wikipedia page on complex numbers for the mathematically curious: https://en.wikipedia.org/wiki/Complex_number
