---
title:                "Writing tests"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests means crafting code that checks if other code works as expected. Programmers do it to catch bugs, ensure reliability, and save headaches later.

## How to:
Clojure uses a library called `clojure.test` to write and run tests. Here's how to use it:

```Clojure
(require '[clojure.test :refer :all])

(deftest addition-test
  (testing "Basic addition"
    (is (= 4 (+ 2 2)))))
    
(run-tests)
```

Sample output after running the test:

```
lein test user
Testing user

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Deep Dive
Clojure's testing approach stems from the REPL-driven development environment. Generative testing with `test.check` and property-based testing are alternative strategies. They auto-generate test cases instead of writing all by hand. Implementation relies heavily on macros, providing a dynamic testing environment. 

## See Also
- [Clojure Testing](https://clojure.org/guides/deps_and_cli#_testing)
- [clojure.test documentation on GitHub](https://github.com/clojure/clojure/blob/master/src/clj/clojure/test.clj)
- [Introduction to property-based testing with `test.check`](https://github.com/clojure/test.check)
