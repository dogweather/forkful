---
title:                "Elixir: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Testdrevet utvikling har blitt en stadig mer populær metode innen programmering, og det med god grunn. Ved å skrive tester før man skriver selve koden, kan man oppdage feil og sørge for at koden fungerer som den skal. Det kan spare deg for mye tid og frustrasjon på lengre sikt.

# Hvordan

For å skrive tester i Elixir, bruker man ofte et rammeverk som heter ExUnit. Her er et eksempel på hvordan man kan skrive en enkel test for en funksjon som legger sammen to tall:

```Elixir
defmodule Math do
  def add(x, y) do
    x + y
  end
end

defmodule MathTest do
  use ExUnit.Case

  test "add funksjonen legger sammen to tall" do
    assert Math.add(2, 3) == 5
  end
end
```

Koden over viser hvordan man kan definere en funksjon i Elixir, og deretter hvordan man kan teste funksjonen ved hjelp av ExUnit. Når man kjører testen, vil man få følgende output:

```
1 test, 0 failures
```

Dette betyr at testen ble vellykket og at funksjonen fungerer som den skal. Man kan også legge til flere tester for å sjekke flere forskjellige scenarioer.

# Dypdykk

Når man skriver tester, er det viktig å tenke på hvilke deler av koden som er mest sårbare for feil. Disse delene bør testes grundigere for å sikre at de fungerer som de skal. Det kan også være lurt å skrive tester for "edge cases", altså situasjoner som kan føre til uventet oppførsel.

I tillegg kan man bruke assert statements til å sjekke at funksjonene returnerer de riktige verdiene eller at de kaster riktig feilmelding når de skal. Man kan også teste sideeffekter av funksjonene, som for eksempel at de oppdaterer en database eller sender ut en e-post.

# Se også

- [ExUnit documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [TDD in Elixir for beginners](http://blog.plataformatec.com.br/2017/01/tdd-in-elixir-for-beginners/)
- [Unit testing your Phoenix controllers](https://blog.drewolson.org/unit-testing-your-phoenix-controllers/)