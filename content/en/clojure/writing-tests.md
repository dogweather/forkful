---
title:                "Clojure recipe: Writing tests"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is a crucial aspect of programming in any language, including Clojure. It allows developers to catch bugs and errors in their code early on, saving time and effort in the long run. Additionally, tests serve as documentation for the code and help ensure that it remains functional even after updates or changes.

## How To

Writing tests in Clojure is made easy with the use of the `clojure.test` library. First, we need to import it into our namespace:

```
(ns my-project.test
  (:require [clojure.test :refer :all]))
```

Next, we can define our test cases using the `deftest` macro and provide a descriptive name for each test. Within the body of the test, we can use standard Clojure functions to check for expected outputs.

```
(deftest test-addition
  (is (= (+ 1 2) 3))
  (is (= (+ 10 5) 15))
  (is (= (+ 7 3) 10)))
```

We can also use the `run-tests` function to run our tests and see the results in the console:

```
(run-tests)

;; Output:
Testing my-project.test

Ran 1 tests containing 3 assertions.
0 failures, 0 errors.
```

## Deep Dive

In addition to using standard Clojure functions such as `=` and `is`, we can also use the `isnt` function to check for negative conditions and `throws?` to check for exceptions. We can also use the `testing` macro to group related tests together.

```
(deftest test-multiplication
  (is (= (* 2 3) 6))
  (testing "Error handling"
    (isnt (throws? (fn [] (* 2 "test"))))))
```

Another useful feature of `clojure.test` is the use of fixtures, which allow us to set up and tear down test environments. This is especially useful for testing database functions or web services.

```
(defn add-user [user]
  ;; Code to add user to database
)

(deftest test-add-user
  (testing "Valid user"
    (is (= (add-user {:username "test" :password "1234"}) true)))
  (testing "Invalid user"
    (is (= (add-user {:username "" :password ""}) false))))

(defn- setup-db []
  (println "Setting up database connection...")
  ;; Code to connect to database
)

(defn- teardown-db []
  (println "Closing database connection...")
  ;; Code to close database connection
)

(use-fixtures :each setup-db teardown-db)

```

## See Also

- [Official documentation for `clojure.test`](https://clojure.github.io/clojure/clojure.test-api.html)
- [Effective testing in Clojure](https://medium.com/swlh/effective-testing-in-clojure-b199d0e589ca)
- [Writing tests with Clojure Spec](https://betweentwoparens.com/articles/writing-tests-with-clojure-spec)