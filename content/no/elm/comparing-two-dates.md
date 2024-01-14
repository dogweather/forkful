---
title:                "Elm: Sammenligning av to datoer"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave i mange programmeringsprosjekter. Når du arbeider med tidsbaserte data, kan det være nyttig å vite hvordan du skal sammenligne to datoer for å kunne håndtere dem riktig.

## Hvordan

For å sammenligne to datoer i Elm, bruker vi funksjonen `Date.compare` som tar inn to datoer og returnerer en `Order` som kan være `LT` (mindre enn), `EQ` (lik) eller `GT` (større enn). Her er et eksempel på hvordan du kan sammenligne to datoer:

```Elm
date1 = Date.fromCalendarDate 2020 11 15
date2 = Date.fromCalendarDate 2020 11 20

Date.compare date1 date2
-- output: LT
```

I dette eksempelet er `date1` mindre enn `date2` siden den kommer først i tidslinjen.

Hvis du trenger å sammenligne kun datoene og ikke tidspunktene, kan du bruke funksjonen `Date.toDays`. Denne funksjonen konverterer datoene til antall dager siden 1970-01-01 og gjør det enklere å sammenligne dem. Her er et eksempel på hvordan du kan bruke `Date.toDays`:

```Elm
date1 = Date.fromCalendarDate 2020 11 15
date2 = Date.fromCalendarDate 2020 11 20

Date.toDays date1
-- output: 18512

Date.toDays date2
-- output: 18517
```

Igjen ser vi at `date1` er mindre enn `date2`.

## Dypdykk

Når vi sammenligner to datoer, er det viktig å tenke på hvordan vi håndterer tidssoner. Elm har en innebygd `Time`-modul som tar vare på tidszoner, men hvis du ønsker å ignorere tidsforskjeller, kan du bruke funksjonen `Date.toTime` som konverterer datoen til millisekunder siden UNIX-epoken (1970-01-01 00:00:00). Her er et eksempel på how vi kan bruke denne funksjonen:

```Elm
date1 = Date.fromCalendarDate 2020 11 15
date2 = Date.fromCalendarDate 2020 11 20

Date.toTime date1
-- output: 1605436800000

Date.toTime date2
-- output: 1605897600000
```

Selv om datoene er fra samme dag, er de forskjellige hvis vi tar hensyn til tidsforskjeller.

## Se også

- [Elm Time module documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Date and Time in Elm by Héctor Ramón](https://becoming-functional.com/date-and-time-in-elm-26cbd0549d98)