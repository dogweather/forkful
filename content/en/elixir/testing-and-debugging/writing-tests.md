---
date: 2024-02-03 19:03:26.395083-07:00
description: "Writing tests in Elixir involves creating automated scripts to validate\
  \ the behavior of your code. Programmers do this to assure quality, prevent\u2026"
lastmod: '2024-03-11T00:14:33.641242-06:00'
model: gpt-4-0125-preview
summary: "Writing tests in Elixir involves creating automated scripts to validate\
  \ the behavior of your code. Programmers do this to assure quality, prevent\u2026"
title: Writing tests
---

{{< edit_this_page >}}

## What & Why?
Writing tests in Elixir involves creating automated scripts to validate the behavior of your code. Programmers do this to assure quality, prevent regressions, and facilitate code refactoring, making the development process more reliable and efficient.

## How to:
Elixir uses ExUnit as its built-in test framework, which is extremely powerful and easy to use. Here's a basic example:

1. Create a new test file in the `test` directory of your Elixir project. For example, if you are testing a module named `MathOperations`, your test file could be `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # This is a simple test case to check the addition function
  test "the addition of two numbers" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

To run your tests, use the `mix test` command in your terminal. If the `MathOperations.add/2` function correctly adds two numbers, you'll see output similar to:

```
..

Finished in 0.03 seconds
1 test, 0 failures
```

For tests involving external services or APIs, you might want to use mock libraries, such as `mox`, to avoid hitting actual services:

1. Add `mox` to your dependencies in `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # other deps...
  ]
end
```

2. Define a mock module in your test helper (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. Use the mock in your test case:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # This tells Mox to verify this mock was called as expected
  setup :verify_on_exit!

  test "gets data from the API" do
    # Setup the mock response
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mocked response"} end)
    
    assert SomeAPIClient.get_data() == "Mocked response"
  end
end
```

When running `mix test`, this setup allows you to isolate your unit tests from real external dependencies, focusing on the behavior of your own code. This pattern ensures your tests run quickly and remain reliable, regardless of external service status or internet connectivity.
