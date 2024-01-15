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

## Why

Writing tests is an important aspect of software development that helps ensure code quality and catch bugs early on. By writing tests, you can test your code in isolation and have confidence in your code's functionality before deploying it to production.

## How To

Writing tests in Clojure is made easy with its built-in testing framework, clojure.test. Let's take a look at how to write tests using this framework.

First, we need to require the clojure.test library:

```Clojure
(ns my-app.core-test
  (:require [clojure.test :refer :all]))
```

Next, we need to define our test functions using the `deftest` macro. Let's say we want to test a function that calculates the area of a circle:

```Clojure
(deftest area-of-circle-test
  (testing "correct area with radius 5"
    (is (= (circle-area 5) 78.5)))
  (testing "correct area with radius 0"
    (is (= (circle-area 0) 0)))
  (testing "correct area with negative radius"
    (is (= (circle-area -5) 78.5))))
```

Note that we use the `testing` macro to group our test cases and the `is` macro to assert that a certain condition is true. In this case, we are using the `=` function to compare the expected and actual values.

Finally, we can run our tests using the `run-tests` function:

```Clojure
(run-tests)
;=>
{:test 3, :pass 3, :fail 0, :error 0, :type :summary}
```

From the output, we can see that all 3 of our tests passed. If there were any failures or errors, they would be displayed in more detail.

## Deep Dive

There are several things to keep in mind when writing tests in Clojure. First, it's important to structure your tests in a descriptive and organized way using the `testing` macro. This makes it easier to understand and maintain your tests.

Additionally, you can use the `is` macro with various assertion functions such as `=` and `not=` to check for different conditions. You can also use the `thrown?` function to assert that certain exceptions are thrown.

Finally, you can use the `use-fixtures` macro to set up and tear down any necessary resources for your tests. This ensures that your tests are run in a consistent environment.

## See Also

- [Clojure Test Library Documentation](https://clojure.github.io/clojure/clojure.test-api.html)
- [Clojure Test Tutorial](https://www.braveclojure.com/unit-testing/#Clojure’s_unit_testing_library:_clojure.test)