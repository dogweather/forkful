---
title:    "Elm: Å få gjeldende dato"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan ofte være nødvendig å hente ut dagens dato i et programmeringsprosjekt. Dette kan være for å vise brukere når noe ble opprettet eller oppdatert, eller for å sammenligne datoer og utføre ulike operasjoner basert på det. I denne bloggposten skal vi se på hvordan man kan få ut dagens dato i Elm-programmering.

## Hvordan

For å få ut dagens dato i Elm bruker vi den innebygde funksjonen `Date.today`. Denne kan kalles på følgende måte:

```Elm
Date.today
```

Funksjonen vil returnere en `Date` type som inneholder dagens dato. For å gjøre dette mer visuelt formatert, kan vi bruke funksjonen `toString` for å konvertere datoen til en lesbar streng:

```Elm
import Date

Date.today
    |> Date.toString
```

Dette vil returnere en streng på formatet "YYYY-MM-DD". Vi kan også få ut deler av datoen ved å bruke funksjoner som `year`, `month` og `day`:

```Elm
Date.today
    |> Date.year
    |> toString
```

```Elm
Date.today
    |> Date.month
    |> toString
```

```Elm
Date.today
    |> Date.day
    |> toString
```

Kombiner gjerne disse funksjonene for å få ut ønsket format.

## Dypdykk

I tillegg til `Date.today` finnes det andre nyttige funksjoner for å håndtere datoer i Elm. `Date.fromParts` lar deg lage en `Date` type ved å gi den et år, en måned og en dag. `Date.fromString` lar deg lage en `Date` type ved å gi den en streng på formatet "YYYY-MM-DD".

Det finnes også funksjoner for å utføre operasjoner på datoer, som for eksempel å legge til eller trekke fra dager, måneder eller år.

## Se også

- [Offisiell Elm dokumentasjon for Date modulen](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Elm-repl for å teste ut forskjellige dato-operasjoner](https://elm-lang.org/try)