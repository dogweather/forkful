---
title:                "Elixir recipe: Writing tests"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Testing is an essential aspect of programming, especially in a language like Elixir where concurrency and fault tolerance are highly valued. By writing tests, you can ensure that your code is functioning properly, catch any bugs early on, and have the confidence to make changes without breaking the code.

## How To

Writing tests in Elixir is relatively easy, thanks to its built-in testing framework called ExUnit. Let's take a look at a simple example of a test for a function that adds two numbers together:

```
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end

defmodule CalculatorTest do
  use ExUnit.Case
  doctest Calculator

  test "add/2 adds two numbers together" do
    result = Calculator.add(2, 3)
    assert result == 5
  end
end
```

In the above code, we define a `Calculator` module with a `add/2` function. Then, in our `CalculatorTest` module, we use the `ExUnit.Case` module and include `doctest Calculator` to automatically generate tests for our functions. We then write a test that asserts the output of `Calculator.add(2, 3)` is equal to 5. 

To run the test, we can use the `mix test` command, and it should return the following output:

```
.....
Finished in 0.02 seconds
5 tests, 0 failures
```

You can see that our test passed successfully! We can continue to write more tests to cover edge cases and ensure our code is robust.

## Deep Dive

Elixir's ExUnit framework provides various useful assert functions such as `assert`, `refute`, `assert_raise`, `assert_receive`, and more. These assert functions allow you to specify the expected outcome of your tests, making them more robust and efficient.

Additionally, ExUnit also supports meta-programming, which enables you to dynamically generate and execute tests. This can be useful when testing functions that have multiple possible outcomes.

Moreover, Elixir has a unique feature called doctests, as seen in our example above. Doctests automatically generate tests based on the code documentation, making it easier to keep the documentation and tests in sync.

## See Also

- [ExUnit documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [An Introduction to Testing in Elixir](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-with.html)
- [The Power of Meta-testing in Elixir](https://medium.com/@rrgalvan/the-power-of-meta-testing-in-elixir-a2bf70005925)

Remember, writing tests not only helps catch bugs but also serves as documentation for your code. So, keep writing tests and happy coding!