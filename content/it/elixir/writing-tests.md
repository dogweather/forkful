---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere test significa automatizzare il controllo di parti di codice per assicurarsi che funzionino come previsto. I programmatori fanno ciò per prevenire bug, facilitare manutenzione e migliorare la qualità del codice.

## How to:
Installazione di ExUnit:
```elixir
# Nel mix.exs, assicurati che ExUnit sia nella sezione 'deps'
defp deps do
  [
    {:ex_unit, "~> 1.12", only: :test}
  ]
end
```

Creiamo un test semplice:
```elixir
# file: test/example_test.exs
defmodule ExampleTest do
  use ExUnit.Case

  test "la somma di 1 e 2 è 3" do
    assert 1 + 2 == 3
  end
end
```

Eseguiamo i test:
```shell
$ mix test
```

Output previsto:
```
Compiling 1 file (.ex)
.

Finished in 0.03 seconds (0.03s async, 0.00s sync)
1 test, 0 failures

Randomized with seed 54321
```

## Deep Dive
ExUnit è il framework di test integrato in Elixir sin dalla sua creazione da José Valim; si ispira a Ruby's MiniTest. Alternativamente, si possono usare strumenti come ESpec per un approccio ispirato a RSpec, o Common Test per compatibilità con Erlang. Implementando i test, viene sfruttata la concorrenza fornita dalla macchina virtuale Erlang per accelerare l'esecuzione.

## See Also
- [Elixir School Testing](https://elixirschool.com/en/lessons/basics/testing/)
- [Documentation for ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Awesome Elixir - A collection of awesome Elixir libraries](https://github.com/h4cc/awesome-elixir#testing)
