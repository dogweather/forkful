---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests is about crafting code that checks other code's correctness. Programmers do it to catch bugs early, ensure quality, and safeguard against future changes breaking things.

## How to:

```Gleam
import gleam/should
import my_module

pub fn my_test() {
  // Checking if the function return the expected value
  should.equal(my_module.my_function(), "expected result")
}

pub fn addition_test() {
  // Testing the addition function for correctness
  should.equal(my_module.add(1, 2), 3)
}
```

Sample output from a successful test suite run:

```
Testing my_module...
  ✓ my_test passes
  ✓ addition_test passes

All tests passed!
```

## Deep Dive

Gleam's testing culture is inspired by its Erlang roots, where robustness is key. Alternatives like property-based testing are also popular in the Erlang ecosystem. Implementation-wise, tests in Gleam are just regular functions with assertions. They're run by a test runner, and results are reported in a human-readable format.

## See Also

- Erlang's common test for context: [http://erlang.org/doc/apps/common_test/basics_chapter.html](http://erlang.org/doc/apps/common_test/basics_chapter.html)
