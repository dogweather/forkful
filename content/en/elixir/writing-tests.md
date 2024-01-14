---
title:    "Elixir recipe: Writing tests"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an essential part of any programming project, and Elixir is no exception. Tests allow us to validate our code, catch bugs early on, and ensure that our code functions as expected. In this blog post, we will explore the importance of writing tests in Elixir and how it can benefit your development process.

## How To

To write tests in Elixir, we use the built-in ExUnit framework. Let's start by creating a simple test file named "my_test.exs". In this file, we will write our first test case:

```elixir
defmodule MyTest do 
  use ExUnit.Case 
  
  test "addition" do 
    assert 1 + 1 == 2 
  end 
end 
```

Here, we declared a module named "MyTest" and used the ExUnit.Case module to define our test cases. In our first test, we added an assertion to check if 1 + 1 equals 2. Now let's run our test by typing `elixir my_test.exs` in the terminal. You should see an output like this:

```
.

Finished in 0.001 seconds 
1 do, 0 failures 
```

The dot indicates that our test was successful, and there were no failures. Let's add another test case to see what happens when something goes wrong:

```elixir
defmodule MyTest do 
  use ExUnit.Case 
  
  test "addition" do 
    assert 1 + 1 == 2 
  end 
  
  test "subtraction" do 
    assert 1 - 1 == 0 
  end 
end 
```

Our new test case will check if 1 - 1 equals 0. Now when we run our test, we should get an output like this:

```
F.

Finished in 0.001 seconds 
1 do, 1 failures 

1) do test subtraction (MyTest) 
test/my_test.exs XMixPanalyzerTest.test_add (module) 
test/my_test.exs:6 
(assert 1 - 1 == 0) 
AssertionError 

Stacktrace 
(test/my_test.exs:6) 
```

As you can see, the first test passed, but the second test failed, showing us an error message and the line where the assertion failed. This is the power of tests, as they help us identify and fix errors in our code.

## Deep Dive

Writing good tests involves more than just adding a few assertions. It requires understanding the purpose and functionality of the code being tested. Tests should be focused, independent, and reproducible. In Elixir, we can use the "setup" and "teardown" functions to set up any required data before running our tests and clean up after each test, respectively. This ensures that our tests are isolated and don't rely on external factors.

We can also use "test tags" to categorize and run specific tests based on their tags. This allows us to run only a specific set of tests during development or in different environments.

Additionally, Elixir has a built-in code coverage tool that helps us track the lines of code covered by tests. This gives us confidence in our test suite and shows us which parts of our code may need more testing.

## See Also

- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Effective Testing in Elixir](https://www.manning.com/books/effective-testing-in-elixir)
- [10 Tips for Writing Clean and Usable Tests in Elixir](https://codeship.com/blog/2017/06/20/10-tips-for-writing-clean-and-usable-tests-in-elixir.html)