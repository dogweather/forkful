---
title:                "Konvertering av dato til en streng"
html_title:           "Elm: Konvertering av dato til en streng"
simple_title:         "Konvertering av dato til en streng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng kan være nyttig når du jobber med ulike typer data i Elm, spesielt når du skal presentere informasjonen til brukeren på en visuelt tiltalende måte.

## Slik gjør du det
For å konvertere en dato til en streng i Elm, kan du bruke funksjonen `toString` sammen med en `Date`-verdi. Her er et eksempel:

```Elm
dateToString : Date -> String
dateToString date =
  toString date
```

I dette eksempelet tar funksjonen `dateToString` en `Date`-verdi som parameter og bruker `toString`-funksjonen til å konvertere den til en streng. Du kan deretter bruke denne funksjonen til å konvertere en dato til en streng hvor som helst i koden din.

```Elm
birthday : Date
birthday =
  Date.fromParts 1990 4 15 0 0 0

birthdayString : String
birthdayString =
  dateToString birthday

-- Output: "1990-04-15"
```

Som du kan se i dette eksempelet, blir fødselsdatoen konvertert til en streng som følger formatet "ÅÅÅÅ-MM-DD". Dette formatet følger ISO 8601-standarden og kan derfor leses og tolkes enkelt av både brukere og andre programmer.

## Dykk dypere
Mens `toString`-funksjonen er enkel og praktisk for å konvertere en dato til en streng, er det også mulig å bruke flere funksjoner for å tilpasse formatet til strengen.

For eksempel, hvis du ønsker å inkludere informasjon om ukedagen i strengen, kan du bruke funksjonen `dayOfWeek` og deretter konvertere denne til en streng ved hjelp av `toString`.

```Elm
dateToString : Date -> String
dateToString date =
  let
    day = dayOfWeek date
    dayString = toString day
  in
  "ÅÅÅÅ-MM-DD, " ++ dayString

-- Output: "1990-04-15, Sunday"
```

I tillegg kan du også bruke funksjoner som `month`, `year` og `hour` for å inkludere mer spesifikk informasjon om datoen i strengen.

## Se også
- Elm Date API: https://package.elm-lang.org/packages/elm/time/latest/Time-Date
- Elm toString dokumentasjon: https://package.elm-lang.org/packages/elm/time/latest/Time-Date#toString