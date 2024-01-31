---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:36:00.126567-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"

category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing av dato fra en streng betyr å tolke tekst og omforme den til et dato-objekt. Programmerere gjør dette fordi datoer i ren tekstform ikke lar oss enkelt utføre datoberegninger eller formatering.

## How to:
I Elm må du ofte installere ekstra pakker for dato-håndtering, for eksempel `justinmimbs/date`.

```Elm
import Date
import Date.Extra.Parse as DateParse

parseDate : String -> Maybe Date.Date
parseDate dateString =
    DateParse.fromIsoString dateString

-- Eksempel på bruk
case parseDate "2023-03-21" of
    Just date ->
        -- Gjør noe med det vellykkede resultet (date er av typen Date.Date nå)
        ...

    Nothing ->
        -- Håndter feilsituasjon, parsing mislyktes.
        ...
```
Output vil være en `Date.Date` hvis teksten er en gyldig ISO-8601 dato.

## Deep Dive
Parsing av datoer har vært en sentral del av programmering lenge, fordi datoer er viktige for logger, hendelser og tidslinjer. I tidligere programmeringsspråk som JavaScript, var dato-parsing ofte bygget inn, men hadde mange inkonsekventer. Elm tar en annen tilnærming og gir ikke parsering ut av boksen – du velger en pakke som passer ditt behov.

`justinmimbs/date` er en populær pakke i Elm-samfunnet og dekker mange vanlige scenarier. En alternativ pakke er `elm-time`, som tilbyr lignende funksjoner.

Implementasjonsmessig støtter `DateParse.fromIsoString` i `justinmimbs/date` ISO-8601, regex-basert tolking, noe som er robust og standardisert.

## See Also
- [justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- [elm-time](https://package.elm-lang.org/packages/elm/time/latest/)
- [ISO-8601 Standard](https://en.wikipedia.org/wiki/ISO_8601)
