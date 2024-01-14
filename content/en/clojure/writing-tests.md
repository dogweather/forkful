---
title:    "Clojure recipe: Writing tests"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why
Writing tests in any programming language is an important aspect of software development. It helps ensure that the code is functioning as expected and can catch any potential errors early on in the development process. For Clojure specifically, writing tests can also aid in maintaining the immutability and purity principles that the language is known for.

## How To 
Writing tests in Clojure follows a similar process to writing tests in other languages. First, you will need to define a namespace and import the necessary libraries. For example:

```Clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))
```

Next, you can define a test using the `deftest` macro, followed by the name of the test and the code that you want to test. For instance:

```Clojure
(deftest test-addition
  (is (= (+ 1 2) 3)))
```

This will create a test called `test-addition` that checks if adding 1 and 2 equals 3. The `is` macro is used to assert the expected result. To run the tests, you can use the `run-tests` function, which will output the results in the terminal.

```
----------------------------------
   Clojure tests (0.016s)  
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.    
```

## Deep Dive
Clojure offers a robust testing library called `clojure.test` that provides various useful functions and macros for testing. Some of these include `is` for asserting equality, `are` for testing multiple values at once, and `throws?` for checking if a specific function throws an exception. Additionally, integration with tools like Leiningen or Boot make it easy to run tests in different environments.

It's also worth noting that writing tests in Clojure can further reinforce functional programming practices. Since the tests themselves are immutable data structures, they can be composed and manipulated just like any other data in Clojure.

## See Also
- [Official Clojure Testing Documentation](https://clojure.org/guides/testing)
- [Clojure Syntax Cheat Sheet](https://clojure.org/api/cheatsheet)