---
title:                "Elm: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har hatt behov for å finne ut hvilken dato det vil være om en viss periode, for eksempel en uke fra nå eller to måneder tilbake i tid, så kan du dra nytte av å lære hvordan man kalkulerer datoer i Elm. Dette kan være nyttig i programmering når man jobber med protokoller som involverer tidsstyring, eller for å lage en praktisk funksjon i en app.

## Hvordan
For å kalkulere en dato i fremtiden eller fortiden i Elm, kan du bruke `add` funksjonen fra `Date` modulen. For eksempel, hvis du ønsker å finne ut hvilken dato det vil være om to måneder fra nå, så kan du bruke følgende kode:

```Elm
import Date exposing (Day)
import Time

Time.now
    |> Date.fromTime
    |> Date.add Time.month 2
    |> Date.toDay
```

Dette vil gi deg et resultat som ligner på: `2019-09-01` i `YYYY-MM-DD` format.

På samme måte, om du ønsker å finne ut hvilken dato det var for to uker siden, kan du bruke følgende kode:

```Elm
import Date exposing (Day)
import Time

Date.fromTime (Time.now)
    |> Date.add Time.week (-2)
    |> Date.toDay
```

Dette vil gi deg et resultat som ligner på: `2019-08-14`.

## Dypdykk
Det er flere parametere som kan brukes sammen med `add` funksjonen for å tilpasse kalkuleringen av datoen. For eksempel, i stedet for å bruke `Time.month` og `Time.week`, kan du bruke `Time.day` for å legge til eller trekke fra et bestemt antall dager.

I tillegg kan du også bruke `Time.inDays` funksjonen til å konvertere tid i millisekunder til dager. Dette kan være nyttig hvis du har en variabel som representerer antall dager fremover eller bakover du ønsker å kalkulere.

## Se også
- [Elm hovedsiden](https://elm-lang.org/)
- [Elm dokumentasjon](https://guide.elm-lang.org/)
- [Offisiell Elm GitHub side](https://github.com/elm/)