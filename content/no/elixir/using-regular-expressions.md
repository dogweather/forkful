---
title:                "Elixir: Å bruke regulære uttrykk"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har prøvd å analysere tekststrenger i et programmeringsspråk, har du kanskje følt deg frustrert over å måtte gjennomgå en lang og tungvint prosess med å finne og manipulere forskjellige deler av strengen. Men med regulære uttrykk (regular expressions) kan du enkelt og effektivt søke etter og endre tekstbaserte mønstre i Elixir.

## Hvordan du bruker regulære uttrykk i Elixir

For å bruke regulære uttrykk i Elixir, må du først importere modulen "Regex". Deretter kan du bruke en av to metoder: Regex.match? og Regex.replace. Regex.match? tar inn et regulært uttrykk og en streng som skal matches, og returnerer en "match" struct dersom uttrykket matcher strengen. La oss si at vi ønsker å finne alle tall i en streng:

```Elixir
Regex.match?(~r/\d+/, "Det var 42 katter på treet")
```

Dette vil returnere en "match" struct med informasjon om at det fant et "tre-sifret" tall i strengen.

Den andre metoden, Regex.replace, tar også inn et regulært uttrykk og en streng, men i tillegg et tredje argument som beskriver hva som skal erstatte matchen. Her kan du også bruke "siktilde" for å referere til matchen i erstatningen. For eksempel, for å bytte ut alle tall med ordet "mange":

```Elixir
Regex.replace(~r/\d+/, "Det var 42 katter på treet", "mange")
```

Dette vil returnere "Det var mange katter på treet". 

## Dypdykk i regulære uttrykk

Det fins en hel del forskjellige syntaksregler og shorthands som kan brukes i regulære uttrykk. For eksempel, kan du bruke "+" for å matche en eller flere forekomster av et mønster, "*" for å matche null eller flere forekomster, og "?" for å gjøre et mønster valgfritt. Du kan også bruke spesialtegn som "." for å matche alle tegn, "^" for å matche begynnelsen av en streng, og "$" for å matche slutten av en streng.

Det fins også flere modifikatorer som kan brukes for å gjøre uttrykket mer presist eller fleksibelt. For eksempel, kan du bruke "i" for å gjøre uttrykket case-insensitive, og "s" for å matche over flere linjer.

En god måte å lære mer om regulære uttrykk er å prøve det ut selv og eksperimentere med forskjellige uttrykk og modifikatorer. Du kan også se på dokumentasjonen for "Regex" modulen for en mer detaljert oversikt over syntaksregler og muligheter.

## Se også

- [Elixir regex dokumentasjon](https://hexdocs.pm/elixir/Regex.html)
- [Regex quick reference cheat sheet](https://www.rexegg.com/regex-quickstart.html#chars) (engelsk)
- [Elixir Learning resources](https://elixir-lang.org/learning.html) (engelsk)