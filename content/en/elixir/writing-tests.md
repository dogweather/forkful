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

Writing tests is a coding practice that involves creating specific scenarios to check if your code behaves as expected. As programmers, we do this to verify our code's functionality, prevent bugs, and streamline debugging.

## How to:

In Elixir, we write tests using a tool named ExUnit. Let's create a small Elixir application for demonstrations. We'll write tests to confirm that a function correctly adds two numbers.

1. Let's begin by initiating a new Mix project named `addition`:
```elixir
mix new addition
```
2. Now, in the `lib/addition.ex` file, define the `Addition.add` function:
```Elixir
defmodule Addition do
  def add(x, y) do
    x + y
  end
end
```
3. Now, let's write our tests in the `test/addition_test.exs`:
```elixir
defmodule AdditionTest do
  use ExUnit.Case, async: true

  test "adding positive numbers" do
    assert Addition.add(2, 2) == 4
  end 

  test "adding negative numbers" do
    assert Addition.add(-2, -3) == -5
  end
end
```
4. Finally, run your tests with:
```elixir
mix test
```
Your tests have passed if your terminal returns:
```elixir
..
Finished in 0.05 seconds
2 tests, 0 failures
```

## Deep Dive

This testing approach took root during the TDD (Test-Driven Development) wave in the late '90s. The concept argues for writing tests before actual code. In Elixir, alternative testing libraries like `Espec`, `ExCheck`, and `ShouldI` exist, though `ExUnit` remains the most commonly used due to its simplicity and powerful assertion capabilities. The `async: true` in the `use ExUnit.Case, async: true` line allows Elixir to run tests concurrently, significantly speeding up the testing process.

## See Also

- Elixir's official ExUnit documentation: [https://hexdocs.pm/ex_unit/ExUnit.html](https://hexdocs.pm/ex_unit/ExUnit.html)
- Tutorial on Test-Driven Development (TDD) with Elixir: [https://code.tutsplus.com/tutorials/test-driven-development-in-elixir--cms-31234](https://code.tutsplus.com/tutorials/test-driven-development-in-elixir--cms-31234)
- Comparison of Elixir testing libraries: [https://www.amberbit.com/blog/2018/9/4/comparison-of-elixir-testing-libraries/](https://www.amberbit.com/blog/2018/9/4/comparison-of-elixir-testing-libraries/)