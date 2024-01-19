---
title:                "Writing tests"
html_title:           "Gleam recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Gleam Programming: Writing Tests - A Quickstart Guide

## What & Why?

Tests help ensure the code behaves as expected. They enable continuity of functionality even with drastic changes, making programmers feel safe and confident while refactoring the code.

## How to:

Writing tests in Gleam is straightforward. Let's start with a simple arithmetic function and its associated test.

```Gleam
pub fn add(a: Int, b: Int) -> Int {
  a + b
}
```

Now, let's write the test for the add function:

```Gleam
import gleam/should
import gleam/int
import my_application/practice

pub fn add_test() {
  practice.add(1, 2)
  |> should.equal(int.success(3))  
}
```
If the test passes, you should see an output like this:

```Gleam
Ok: Passed 1 tests. 
```

If the test fails, you'll get an error message pointing out where the test went wrong.

## Deep Dive

Gleam, created in 2018, built a hobby project by Louis Pilfold, borrows its pattern from React, Elm, and Ruby testing. The core idea is to ensure code correctness.

While Gleam makes unit testing simpler, alternatives like QuickCheck bring generative testing to the table. With QuickCheck, programmers can generate extensive sets of random tests to explore the application's behavior.

Taking a closer look at our test, we use Gleam's imported libraries - `gleam/should` to format the test, `gleam/int` to handle integer operations, and our own module under test - `my_application/practice`. Then, run the function under test, add(1, 2), pipe the result to `should.equal` and check it against what we expect `int.success(3)`.

## See Also

While this is a quick introduction to writing tests in Gleam, each aspect discussed has depth. Dive into these resources for detailed knowledge:

- Dive deeper into Gleam Programming at [Gleam Language](https://gleam.run/)
- Learn more about testing in Gleam with the [Gleam Docs](https://gleam.run/book/tour/tests.html).
- Explore generative testing with [QuickCheck](http://www.quviq.com/products/erlang-quickcheck/).