---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in programming is crafting code to check if other code works properly. Programmers do it to catch bugs early, confirm new features don't break old ones, and sleep better knowing their code is solid.

## How to:

In Elixir, you'll use ExUnit to write tests. It's a built-in framework that's friendly and easy to get started with. Here's a quick example:

```elixir
# test/example_test.exs
defmodule ExampleTest do
  use ExUnit.Case

  test "the truth" do
    assert 1 + 1 == 2
  end
end
```

Run it with `mix test`:

```shell
$ mix test
..

Finished in 0.03 seconds
1 test, 0 failures
```

Nice! You wrote a test that confirms math hasn't changed.

## Deep Dive

Testing's been a big deal in Elixir since José Valim gave life to the language, inspired by Ruby's testing culture. Alternatives? Not many within Elixir's world – ExUnit is the go-to. However, you might explore property-based testing with StreamData or dive into mocking with Mox for more complex scenarios. Tests are all about asserting expected outcomes—which you've seen with `assert`—but there's also `refute` for specifying what shouldn't happen.

## See Also

To grow your test-writing skills, check these out:

- Elixir's testing guides: https://hexdocs.pm/ex_unit/ExUnit.html
- StreamData for property-based testing: https://hexdocs.pm/stream_data/StreamData.html
- Mocking with Mox: https://hexdocs.pm/mox/Mox.html

Now go test-drive some code!
