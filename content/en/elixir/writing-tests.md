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

## What & Why?
Writing tests in Elixir is the process of creating automated test cases to check the functionality and correctness of your code. Programmers do this to ensure that their code works as expected and to catch any potential bugs or errors before they become larger issues.

## How to:
Writing tests in Elixir is a simple and straightforward process. First, you need to create a test file with the `.exs` extension. Within this file, you can define your test cases using the `test` macro. For example:

```
Elixir
test "adding 2 and 2 equals 4" do
  result = Calculator.add(2, 2)
  assert result == 4
end
```

This code block creates a test case for a `Calculator` module, where we expect the add function to return 4 when given the numbers 2 and 2. The `assert` statement checks if the result is equal to the expected value. You can run this test file using the `mix test` command and see the output of each test case.

## Deep Dive:
Writing tests is not a new concept in programming. It is a fundamental part of the Test Driven Development (TDD) approach, where tests are written before the actual code to guide the development process. Other alternatives to this approach include Behavior Driven Development and Acceptance Test Driven Development. Elixir provides a built-in testing framework called ExUnit, which makes writing tests even easier. You can also use external libraries like ExSpec to write more expressive and descriptive tests.

## See Also:
- Official Elixir documentation for [ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School](https://elixirschool.com/en/lessons/advanced/testing/) lesson on testing in Elixir
- [ExSpec](https://github.com/antonmi/exspec) library for more readable tests in Elixir