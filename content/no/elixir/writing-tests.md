---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Tester sikrer koden din fungerer som den skal. Programmerere skriver tester for å fange feil tidlig og gjøre koden lettere å vedlikeholde.

## How to:
Basistester i Elixir med ExUnit:
```elixir
defmodule MathTest do
  use ExUnit.Case
  doctest Math

  test "adds two numbers" do
    assert Math.add(1, 2) == 3
  end
end
```
Kjør testene med `mix test`, og forvent følgende:
```
...

Finished in 0.03 seconds
1 test, 0 failures
```

## Deep Dive
Elixir's testsystem, ExUnit, er en kjernekomponent fra start. Alternativer som ESpec tilbyr RSpec-lignende syntaks. Testdrevet utvikling (TDD) anbefales, hvor tester skrives før implementering.

## See Also
- [Elixir's ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Introduction to Mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)