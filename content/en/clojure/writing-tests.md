---
title:                "Writing tests"
html_title:           "Clojure recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests is the process of creating code that checks the functionality of other code, also known as "test code". Programmers write tests to ensure that their code functions as intended, catches potential bugs, and maintains its functionality over time.

## How to: 

Writing tests in Clojure is easy using the built-in test framework called `clojure.test`. Here's an example of a test for a function that adds two numbers:

```Clojure
(require '[clojure.test :refer [deftest is]])
(deftest test-addition
  (is (= 5 (+ 3 2))))
```

The above code creates a test called `test-addition` using the `deftest` macro. Within the test, we use the `is` macro to compare the expected result of adding 3 and 2 to the actual result. If the two values don't match, the test will fail. Here's the output when we run the test:

```
FAIL in (test-addition) (form-init7499349873952303573.clj:3)
expected: (= 5 (+ 3 2))
  actual: (not (= 5 6))
```

As you can see, the test failed because 5 does not equal 6. This indicates that there is an error in our code. Writing tests can help us catch these errors and ensure that our code is functioning correctly.

## Deep Dive

Writing tests has become an essential practice in modern software development due to its many benefits. It not only helps catch bugs, but it also serves as documentation for how the code is meant to function. Writing tests also allows for more efficient debugging and makes it easier to refactor code without breaking its functionality. While there are other testing frameworks available for Clojure, `clojure.test` is the official one and is widely used.

## See Also

- [Official Clojure documentation for `clojure.test`](https://clojure.github.io/clojure/clojure.test-api.html)
- [Practical Guide to Clojure Test Driven Development](https://purelyfunctional.tv/guide/clojure-test-driven-development/)