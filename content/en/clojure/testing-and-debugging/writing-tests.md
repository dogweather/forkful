---
date: 2024-02-03 19:03:16.905534-07:00
description: "Writing tests in Clojure, much like in other programming languages,\
  \ involves creating dedicated code to verify that your main codebase works as expected.\u2026"
lastmod: '2024-03-13T22:44:59.748882-06:00'
model: gpt-4-0125-preview
summary: "Writing tests in Clojure, much like in other programming languages, involves\
  \ creating dedicated code to verify that your main codebase works as expected.\u2026"
title: Writing tests
---

{{< edit_this_page >}}

## What & Why?
Writing tests in Clojure, much like in other programming languages, involves creating dedicated code to verify that your main codebase works as expected. It helps in ensuring correctness, facilitating refactoring, and enhancing code stability.

## How to:
Clojure, leveraging the JVM, supports various testing frameworks. However, a commonly used built-in library is `clojure.test`. Here's a simple example:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Addition functionality"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
After running this test, you would see an output similar to:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

For those seeking more feature-rich options, one can utilize third-party libraries like `Midje` or `test.check`. Here's how you might use Midje for a similar test:

First, add Midje to your project.clj dependencies:
```clojure
[midje "1.9.9"]
```

Then, your test with Midje might look like this:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Testing addition"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Upon running the test through Midje with `lein midje`, the output would display something akin to:

```
All checks (2) succeeded.
```
