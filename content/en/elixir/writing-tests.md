---
title:                "Elixir recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Why Writing Tests is Important in Elixir

Writing tests is an essential part of any software development process, and Elixir is no exception. By writing tests, you can ensure that your code runs as intended and avoid potential bugs and errors. Additionally, tests serve as documentation for your code, making it easier for other developers to understand and contribute to your project. Overall, writing tests allows for a more reliable and efficient development process.

## How To Write Tests in Elixir

To write tests in Elixir, we will be using the built-in testing framework, ExUnit. Let's start by creating a new file called "calculator_test.exs" and adding the following code:

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "addition test" do
    assert Calculator.add(2, 3) == 5
  end
end
```

In this test, we are testing the "add" function in our calculator module. We use the "assert" macro to check if the output of our function matches the expected result. To run this test, we can use the command `mix test` in our terminal. If all goes well, we should see a passing test! 

## Deep Dive into Testing in Elixir

One of the unique features of Elixir's testing framework is the use of "doctests" in addition to traditional unit tests. Doctests allow for documentation and testing to be combined, making it easier to keep your documentation up to date. We can add doctests to our calculator module by modifying the code as follows:

```Elixir
defmodule Calculator do
  @moduledoc """
  A module for basic calculations.
  """

  @doc """
  Adds two numbers together.
  
  ## Examples

      iex> Calculator.add(2, 3)
      5
  
  """
  def add(x, y) do
    x + y
  end
end
```

Now, when we run `mix test`, we will see both our unit test and doctests being executed. This allows for more comprehensive testing and documentation within our code.

## See Also
- [ExUnit documentation](https://hexdocs.pm/ex_unit/ex_unit.html)
- [Elixir School's guide on testing](https://elixirschool.com/en/lessons/basics/testing/)
- [Testing in Elixir: A Comprehensive Guide](https://thoughtbot.com/blog/testing-in-elixir-a-comprehensive-guide)