---
title:                "Writing tests"
html_title:           "Elixir recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests is an essential part of software development, regardless of the language used. It ensures that the code works as expected and catches any potential bugs. In Elixir, testing also plays a crucial role in maintaining the reliability and fault-tolerance of the code, making it a necessary practice for any Elixir developer.

## How To
To start writing tests in Elixir, you first need to create a `test` directory in your project. This is where all your test files will be located. In each test file, you can write tests using the `defmodule` and `deftest` macros. Here's an example:

```Elixir
defmodule MathTest do
  use ExUnit.Case
  # 1
  test "addition" do
    # 2
    assert 1 + 2 == 3
  end
end
```

1. `use ExUnit.Case` provides the necessary testing functionality.
2. Inside the `test` macro, you can write your test case using the `assert` macro.

Once you have written your test cases, you can run them using the command `mix test` in your terminal. You will see the output of each test case, along with a summary of the total number of tests passed and failed.

## Deep Dive
Elixir's testing framework, ExUnit, provides several macros and assertions to help write effective tests. Some of the common macros include `setup` and `teardown` which are run before and after each test respectively. This helps with setting up any necessary data or environment for testing.

When writing more complex tests, you can also use the `ExUnit.CaptureIO` module to capture and test outputs to the console or file.

One of the key principles of testing in Elixir is the concept of "mocking". This involves replacing external or expensive calls with simple and predictable responses to isolate the behavior being tested. The `ExUnit.Callback` module provides utilities for mocking functions.

## See Also
- [ExUnit documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School article on testing](https://elixirschool.com/en/lessons/basics/testing/)
- [ElixirConf talk on writing maintainable tests](https://www.youtube.com/watch?v=5hbas0iZ6wg)