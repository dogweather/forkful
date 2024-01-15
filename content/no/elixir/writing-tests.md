---
title:                "Å skrive tester"
html_title:           "Elixir: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal man bruke tid på å skrive tester? Vel, det er flere grunner til det. Først og fremst vil tester bidra til å sikre kvaliteten på kildekoden din. Det vil også hjelpe deg med å identifisere eventuelle feil og sørge for at alt fungerer som det skal. I tillegg vil det gjøre det enklere å vedlikeholde og videreutvikle koden din i fremtiden.

## Hvordan

For å skrive tester i Elixir, trenger du først å opprette en `test` mappe i prosjektet ditt. Deretter kan du lage en `test.exs` fil hvor du kan definere og kjøre tester. La oss se på et eksempel:

```Elixir
defmodule Calculator do
  def add(x, y) do
    x + y
  end
end

ExUnit.start()

defmodule CalculatorTest do
  use ExUnit.Case

  test "adds two numbers" do
    assert Calculator.add(2, 3) == 5
  end
end
```

I dette eksempelet har vi opprettet en enkel kalkulatorfunksjon og en tilhørende test som sjekker om funksjonen returnerer riktig svar. Ved å kjøre testen, vil vi få en output som sier at testen har passert.

## Deep Dive

Å skrive tester bygger på prinsippet om "Test Driven Development" (TDD). Dette innebærer å først skrive tester som skal svikte, og deretter skrive kode som består testene. Dette hjelper deg med å fokusere på den eksakte funksjonaliteten du ønsker å implementere, og fører ofte til bedre og mer strukturert kode.

I Elixir kan du også bruke biblioteker som `ExUnit` og `ExSpec` for å strukturere og organisere tester på en mer effektiv måte. I tillegg til å teste funksjonaliteten til koden din, kan du også skrive tester for å sjekke input/output, forventet feilhåndtering og ytelse.

## Se også

- [Elixir Testing: From Beginner to Master](https://www.codementor.io/@joshuaballoch/elixir-testing-from-beginner-to-master-34hm3wrfu)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [ExSpec Documentation](https://hexdocs.pm/exspec/ExSpec.html)