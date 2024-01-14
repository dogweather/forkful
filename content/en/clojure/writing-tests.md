---
title:    "Clojure recipe: Writing tests"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an essential part of developing any software project, whether it's a small personal project or a large scale application. It allows developers to ensure that their code is functioning as expected, catching any bugs or errors before they reach production. Writing tests also helps with maintaining code quality, allowing for easier identification of any issues that may arise in the future.

## How To

To write tests in Clojure, we will be using the built-in testing framework, Clojure.test. This framework provides functions to create tests, run them, and report the results. Let's walk through an example of writing a simple test for a function that adds two numbers.

```Clojure
;; Define the function to be tested
(defn add [x y]
  (+ x y))

;; Import the testing library
(use 'clojure.test)

;; Define a test case
(deftest test-add
  (testing "add two positive numbers"
    (is (= (add 3 4) 7))))

;; Run the tests
(run-tests)
```

If the test passes, we will see the following output:

```
Testing user
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

If the test fails, we will see a message indicating which assertion failed and expected vs. actual values.

```
Testing user
Ran 1 tests containing 1 assertions.
1 failures, 0 errors.
----Testing for add----
expected: (= (add 3 2) 5)
  actual: (not (= 5 5))
```

## Deep Dive

When writing tests, it's essential to cover all possible scenarios to ensure thorough testing. Clojure provides several functions for testing, including `is`, `are`, and `are-not`. These functions take two arguments, the expected value and the actual value, and compare them. Other functions, such as `pass?` and `fail?`, can be useful for more complex tests that require custom logic.

It's also important to note that tests should be written to be independent of one another, meaning they should not rely on any previous tests for their expected outcome. This ensures that each test can run independently of the others, providing more accurate results.

## See Also

* Official Clojure documentation for testing: https://clojure.org/guides/testing
* Clojure.test API reference: https://clojure.github.io/clojure/clojure.test-api.html
* Blog post on why writing tests is important: https://blog.cleancoder.com/uncle-bob/2017/04/05/BadTesting.html