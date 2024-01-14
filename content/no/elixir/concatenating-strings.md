---
title:                "Elixir: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konkatenerre strenger (mer kjent som å sammenføye eller sette sammen strenger) er en vanlig oppgave i mange programmeringsspråk, inkludert Elixir. Det kan være nyttig å lære å gjøre dette for å håndtere dynamiske data eller for å lage mer komplekse tekst utdata.

## Hvordan

```Elixir
name = "Kristine"
age = 28
occupation = "programmerer"

IO.puts "Hei, mitt navn er #{name}, jeg er #{age} år gammel og jeg jobber som #{occupation}."
```

Output:
```
Hei, mitt navn er Kristine, jeg er 28 år gammel og jeg jobber som programmerer.
```

I eksempelet over har vi definert variabler for navn, alder og yrke. Ved å bruke interpolasjon, som er symbolisert med `#{}` inne i en streng, kan vi sette inn verdien av variablene i strengen. Det resulterende utdata vil være en setning som inneholder den dynamiske informasjonen som er satt sammen.

```Elixir
quote = "Elixir brings joy to programming."

IO.puts "Remember: #{quote}"
```

Output:
```
Remember: Elixir brings joy to programming.
```

Her bruker vi også interpolasjon for å sette inn en variabel i en annen streng, og resultater er en korrekt sammensatt setning med dynamisk informasjon.

## Deep Dive

Å konkatenerre strenger kan også oppnås ved å bruke operatoren `<>`, som står for "kontatenere". Denne måten kan være mer brukervennlig hvis du jobber med flere strenger eller en samling av strenger. Du kan også bruke `<<>>` for å konkatenerre binære data, men vi vil fokusere på strenger her.

```Elixir
name = "Michael"
age = "41"
occupation = "musiker"

concert = "Jeg heter " <> name <> " og jeg er " <> age <> " år gammel. Jeg spiller musikk som en " <> occupation <> "."

IO.puts concert
```

Output:
```
Jeg heter Michael og jeg er 41 år gammel. Jeg spiller musikk som en musiker.
```

Det er viktig å merke seg at begge måtene å konkatenerre strenger på returnerer en ny streng og påvirker ikke de opprinnelige strengene.

## Se Også

- [Offisiell Elixir Dokumentasjon](https://elixir-lang.org/getting-started/basic-types.html#strings)
- [Elixir School - Strenger](https://elixirschool.com/lessons/basics/strings/)
- [Elixir String Modul](https://hexdocs.pm/elixir/String.html)