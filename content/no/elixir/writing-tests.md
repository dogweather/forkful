---
title:    "Elixir: Å skrive tester"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av Elixir-programmering fordi det hjelper deg med å identifisere og fikse feil før de kommer ut i produksjon. Dette sparer deg for tid og ressurser og øker kvaliteten på koden din.

## Hvordan

For å skrive tester i Elixir, begynn med å opprette en testmodul ved å bruke `ExUnit.Case`-makroen:

```Elixir
defmodule MinModulTest do
  use ExUnit.Case
end
```

Deretter kan du definere testfunksjoner ved hjelp av `test/2`-makroen og gi testen et passende navn:

```Elixir
defmodule MinModulTest do
  use ExUnit.Case

  test "test av addisjonsfunksjon" do
    assert 2 + 2 == 4
  end
end
```

Kjør testen ved å kjøre kommandoen `mix test` i terminalen. Resultatet skal bli:

```
Finished in 0.04 seconds
1 test, 0 failures
```

Du kan også definere flere tester innenfor samme testfunksjon ved hjelp av `do`-blokk og `assert`-makroen:

```Elixir
defmodule MinModulTest do
  use ExUnit.Case

  test "test av multiplikasjonsfunksjon" do
    assert 2 * 3 == 6

    assert 4 * 5 == 20
  end
end
```

På denne måten kan du teste flere ulike verdier og tilstander for samme funksjon.

## Deep Dive

Elixir-tester kan også inkludere setup- og teardown-funksjoner som kjøres før og etter hver test. Dette gjøres ved å bruke `setup/2`- og `teardown/2`-makroene:

```Elixir
defmodule MinModulTest do
  use ExUnit.Case

  setup do
    [a: 1, b: 2]
  end

  test "testing av variabler", %{a: a, b: b} do
    assert a + b == 3
  end

  teardown do
    IO.puts "Testen er ferdig."
  end
end
```

I tillegg kan du også inkludere modulspesifikke funksjoner og hjelpefunksjoner i testmodulen for å gjøre testene dine mer effektive og organiserte.

## Se også

- [Elixir Dokumentasjon om Testing](https://hexdocs.pm/elixir/ExUnit.html)
- [Testing Your Elixir Application](https://pragprog.com/titles/ltpelixir/testing-elixir/)