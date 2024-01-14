---
title:                "Elixir: Skrive tester"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å være en god Elixir-programmerer. Det hjelper deg med å sikre at koden du skriver fungerer som den skal og unngå feil når du endrer eller legger til ny kode. Det gir også et godt fundament for å forstå koden din bedre og gjøre den mer lesbar for andre utviklere.

## Hvordan

Når du skriver tester i Elixir, bruker du ofte biblioteket ExUnit, som er et testrammeverk som følger med Elixir-installasjonen. La oss se på et enkelt eksempel:

```Elixir 
defmodule KalkulatorTest do
  use ExUnit.Case
  test "addisjon" do
    assert 1 + 1 == 2
  end
end
```
For å kjøre testen, kan du bruke kommandoen `mix test`. Outputen vil være noe som dette:

```
1) test addisjon (KalkulatorTest)
   test/kalkulator_test.exs:4
   Assertion with == failed
   code: 1 + 1 == 2
   left:  2
   right: 1
   stacktrace:
     test/kalkulator_test.exs:5: (test)
```

Her ser vi at testen feilet fordi vi forventet at 1+1 skulle være lik 2, men det var ikke tilfelle. Dette er et enkelt eksempel, men du kan også bruke mer komplekse tester for å sjekke at funksjoner og metoder oppfører seg som de skal.

## Deep Dive

Når du skriver tester, er det viktig å dekke så mange ulike scenarier og kanttilfeller som mulig. Det er også nyttig å følge prinsippet om "enhetstesting", hvor du tester hver enkelt funksjon eller metode separat for å sikre at den oppfører seg som den skal. På denne måten kan du isolere og identifisere eventuelle feil mer effektivt.

En annen fordel med å skrive tester er at det gjør det enklere å vedlikeholde og refactorere koden din senere. Når du legger til ny funksjonalitet eller endrer noe, kan du kjøre testene for å sikre at alt fortsatt fungerer som det skal. Dette bidrar til å opprettholde kvaliteten på koden over tid.

## Se Også

- [Offisiell Elixir ExUnit-dokumentasjon](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School: Testing](https://elixirschool.com/en/lessons/basics/testing/)