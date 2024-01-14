---
title:                "Elixir: Lesing av kommandolinje-argumenter"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen bry seg om å lese kommandolinjeargumenter? Vel, det er et viktig aspekt av å skrive Elixir-programmer som kan gjøre dem mer fleksible og brukervennlige.

I stedet for å hardcoded konstanter eller variabler direkte i koden, kan vi la brukeren spesifisere disse verdiene ved å lese kommandolinjeargumenter. Dette gjør det mulig for brukeren å tilpasse programmet etter deres behov uten å måtte endre koden.

## Hvordan gjøres dette

Først må vi importere modulen `System` ved å legge til `use System` øverst i filen vår. Dette vil gi oss tilgang til funksjonene som lar oss lese kommandolinjeargumenter.

Så kan vi bruke funksjonen `argv` fra `System`-modulen for å få en liste over kommandolinjeargumentene. Hvis vi for eksempel vil lese et tall som brukeren angir som det første argumentet, kan vi gjøre det slik:

```elixir
input = System.argv() |> List.first() |> String.to_integer()
```

Vi bruker `List.first()` for å få det første elementet i listen og `String.to_integer()` for å konvertere det til et heltall.

Vi kan også lese mer komplekse argumenter, som for eksempel en liste med strenger, ved å bruke regex-mønstre og funksjonen `Regex.scan/2` fra `Regex`-modulen.

## Dypdykk

Når vi bruker kommandolinjeargumenter, er det viktig å håndtere eventuelle feil som kan oppstå. For å gjøre dette kan vi bruke `try/catch`-blokker og håndtere de forskjellige typer feil som kan oppstå.

Vi kan også bruke standardverdier i tilfeller der brukeren ikke angir et kommandolinjeargument. Dette kan være spesielt nyttig hvis vi ønsker å ha en fallback-verdi hvis brukeren ikke angir noe.

## Se også

- [Elixir System-modulen](https://hexdocs.pm/elixir/System.html)
- [Elixir Regex-modulen](https://hexdocs.pm/elixir/Regex.html)
- [Offisiell Elixir-dokumentasjon](https://elixir-lang.org/docs.html)