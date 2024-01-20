---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests schreiben bedeutet, Code zu verfassen, der deinen Programmcode automatisch ausführt und überprüft, damit er wie erwartet funktioniert. Programmierer testen ihren Code, um Fehler frühzeitig zu erkennen, die Qualität zu sichern und den Wartungsaufwand zu minimieren.

## How to:

Elixir verwendet ExUnit für Tests, ein Framework, das mitgeliefert wird. So schreibst du Tests:

```elixir
# in test/example_test.exs
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  test "die Summe von 1 und 2" do
    assert 1 + 2 == 3
  end

  test "Multiplikation ist kommutativ" do
    assert 2 * 3 == 3 * 2
  end
end
```

Ausführen mit `mix test`. Beispielhafte Ausgabe:

```
..

Finished in 0.03 seconds
2 tests, 0 failures
```

## Deep Dive

Elixir's Testkultur stammt aus Erlang und der funktionalen Programmierung. Alternativen wie Property Based Testing mit Libraries wie StreamData gibt's auch. ExUnit läuft in einer eigenen VM Instanz, um Seiteneffekte zu vermeiden. Details: Tests nutzen `assert` für Überprüfungen und können mit `setup` vorbereitet werden.

## See Also

- [Elixir School's Guide to ExUnit](https://elixirschool.com/en/lessons/basics/testing/)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Intro to Property Based Testing in Elixir](https://elixir-lang.org/blog/2017/10/31/stream-data-property-based-testing-and-data-generation-for-elixir/)