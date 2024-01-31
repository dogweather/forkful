---
title:                "Pisanie testów"
date:                  2024-01-19
simple_title:         "Pisanie testów"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testowanie to sprawdzanie czy kod robi to co powinien. Robimy to by zapobiec błędom, ułatwić zmiany i podnieść jakość kodu.

## How to:
```elixir
defmodule ExampleTest do
  use ExUnit.Case, async: true

  test "the truth" do
    assert 1 + 1 == 2
  end

  test "list operations" do
    assert Enum.reverse([1, 2, 3]) == [3, 2, 1]
  end
end
```
Przykładowy wynik:
```
..

Finished in 0.03 seconds
2 tests, 0 failures
```

## Deep Dive
Testowanie w Elixirze zaczęło się od ExUnit, wbudowanego frameworka. Opcje to między innymi ESpec – wzorowany na RSpec, czy StreamData do property-based testing. W ExUnit każdy `test` to oddzielna funkcja z własnym stanem, używając `assert` sprawdzamy wyniki.

## See Also
- [ExUnit - Elixir School](https://elixirschool.com/pl/lessons/basics/testing/)
- [Elixir testing - HexDocs](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Property-based testing - HexDocs](https://hexdocs.pm/stream_data/StreamData.html)
