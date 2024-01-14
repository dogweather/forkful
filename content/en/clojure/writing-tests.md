---
title:                "Clojure recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Testing is an essential part of any programming project, and Clojure is no exception. Writing tests allows us to catch errors early on, ensure our code is functioning properly, and make future changes with confidence. It may seem like extra work, but in the long run, it will save time and headaches.

## How To
Writing tests in Clojure is easy and straightforward. We will be using the built-in library "clojure.test" to create our tests. Let's take a simple example of a function that adds two numbers and returns the result.

```Clojure
(ns my-project.core-test
  (:require [clojure.test :refer :all]
            [my-project.core :refer :all]))

(deftest test-add
  (is (= 6 (add 2 4))))
```

In this example, we have created a test named "test-add" which uses the "is" assertion to check if the result of our "add" function is equal to 6 when given the inputs 2 and 4. 
We can run this test by calling "lein test" in our project directory. If the test passes, we will see an output of "Ran 1 tests containing 1 assertions. 0 failures, 0 errors." 
But what if we want to test multiple scenarios or functions? We can use the "testing" macro to group multiple tests together.

```Clojure
(deftest test-multiply
  (testing "positive multiplication"
    (is (= 8 (multiply 2 4))))
  (testing "negative multiplication"
    (is (= -8 (multiply -2 4))))
  (testing "zero multiplication"
    (is (= 0 (multiply 0 4)))))
```

We can also use the "testing" macro to provide descriptive names for our tests, making it easier to identify any failing tests.

## Deep Dive
Now that we have covered the basics, let's take a deeper look at writing tests in Clojure. The "clojure.test" library provides various assertion functions, such as "is", "are", "testing", "thrown?", etc. These functions allow us to check different conditions, such as equality, exceptions, etc. You can find the full documentation for these functions [here](https://clojure.github.io/clojure/clojure.test-api.html).

It is also a good practice to organize our tests into different namespaces, just like we do with our code. This helps to keep our tests separate from our actual code, making it easier to maintain and debug.

Lastly, we can also use third-party libraries such as "midje" or "spectrum" for more advanced testing features, such as mocking and property-based testing.

## See Also
- [Clojure Official Documentation on Testing](https://clojure.org/guides/testing)
- [Article on Writing Tests in Clojure](https://www.braveclojure.com/practicalli-testing-clojure)
- [Introduction to clojure.test](https://www.stevejgordon.co.uk/getting-started-with-clojure-test-writing-tests-in-clojure)

Remember, testing is an important step in the development process, and it will benefit you in the long run. So don't skip it!