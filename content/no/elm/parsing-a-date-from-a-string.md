---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse en dato fra en streng betyr å omdanne en skriftlig tekst til en dataobjekt. Dette gjør det lettere for programmerere å jobbe med dataene, for eksempel ved å manipulere, sammenligne, sortere og lagre datoer i databaser.

## Hvordan gjøre det:
Her er en enkel kode i Elm for å parse en dato fra en streng.

```Elm
import Date 
import Time

datumString : String
datumString = "2022-02-28"

main = 
    let
        maybeDate = Date.fromString datumString
    in
    case maybeDate of 
        Nothing -> Debug.log "Feil ved parsing av dato"
        Just date -> Debug.log ( "Dato er: " ++ (Date.toIsoString date))

```
Kjører du denne koden, vil utskriften bli:

```
Dato er: 2022-02-28
```

## Dyp Dykk
Over tid har forskjellige programmeringsspråk utviklet egne måter å parse datoer på. I eldre programmeringsspråk som C og Python, er det vanlig å bruke funksjoner som `strptime`. I moderne språk som Elm, bruker vi metoden `fromString`.

Det finnes også alternativer til å bruke innebygde funksjoner for parsing av datoer, som bibliotekene Moment.js (i JavaScript) eller joda-time (i Java).

Når det gjelder implementeringsdetaljer, vil `Date.fromString` prøve å konvertere en streng til en dato ved vat tolke strengen som ISO 8601. Hvis parsing mislykkes, returnerer funksjonen `Nothing`. Hvis det er vellykket, returneres en `Just` som inneholder den parsede datoen.

## Se også:
Elm dokumentasjon for Date.fromString: https://package.elm-lang.org/packages/elm/time/latest/Time-Posix#fromIsoString

Google Developers' artikkel om hvordan å parse datoer: https://developers.google.com/web/updates/2011/10/Simpler-datetime-parsing-with-getTime