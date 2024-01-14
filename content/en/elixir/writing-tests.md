---
title:    "Elixir recipe: Writing tests"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why
Writing tests is an essential part of the software development process. It ensures that our code works as intended and continues to work even when changes are made. In Elixir, testing is made easy with its built-in testing framework, ExUnit. In this blog post, we will explore the importance of writing tests and how to do it effectively in Elixir.

## How To
To write tests in Elixir, we first need to create a test file named `filename_test.exs`, where `filename` is the name of the module we want to test. Inside this file, we need to define a test case using the `ExUnit.Case` module:

```Elixir
defmodule MyModuleTest do
  use ExUnit.Case

  test "adds two numbers" do
    result = MyModule.add(2, 3)

    assert result == 5
  end
end
```

In the above example, we define a test named "adds two numbers" and use the `assert` macro to verify if the result of calling the `add` function from our `MyModule` is equal to 5.

To run our tests, we can use the `mix test` command in the terminal. This will execute all the tests in our project and show us the results.

```
$ mix test
MyModuleTest
  * test adds two numbers (0.00ms) (passed: 1)
```

If a test fails, the output will show us the reason for the failure, making it easier for us to debug and fix the issue. 

## Deep Dive
Writing effective tests requires us to think about different scenarios and edge cases that might cause our code to fail. Elixir provides us with helpful tools to test these cases, such as the `assert_raise` macro, which allows us to test if a certain function throws an error.

```Elixir
test "divides two numbers" do
  assert_raise ZeroDivisionError, fn -> MyModule.divide(6, 0) end
end
```

We can also use the `setup` and `teardown` functions to run code before and after each test, respectively. This is useful when we need to set up certain conditions for our tests or clean up after running them.

Additionally, we can also use the `ExUnit.Assertions` module to access more assertion macros, such as `assert_match` for testing if a string matches a regular expression or `refute` for negating an assertion.

## See Also
- Elixir School: [Testing with ExUnit](https://elixirschool.com/en/lessons/basics/testing/)
- Plataformatec Blog: [ExUnit tips: setup and teardown callbacks](https://blog.plataformatec.com.br/2015/10/how-to-organize-your-tests-in-elixir/)
- The Elixir Programming Language: [ExUnit](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-with.html#exunit)