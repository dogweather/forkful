---
title:                "Analysering av en dato fra en streng"
aliases:
- /no/elm/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:20.239495-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av en dato fra en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å tolke en dato fra en streng i Elm involverer konvertering av tekstlig informasjon som representerer datoer og tider til et format som Elm kan forstå og manipulere, spesifikt til `Date`-typen. Denne prosessen er avgjørende for å håndtere brukerinndata, vise datoer korrekt lokalisert og utføre dato-relaterte beregninger, noe som sikrer at Elm-applikasjonene dine kan behandle tidsrelatert data intelligent.

## Hvordan:
Elm har ikke innebygde evner som er like robuste som noen andre språk for datotolking, og støtter seg hovedsakelig på Javascript interop eller biblioteker for mer komplekse operasjoner. Imidlertid kan du bruke `elm/time`-pakken for grunnleggende tolking, og for mer komplekse behov er det tredjepartsbiblioteket `justinmimbs/date` sterkt anbefalt.

### Tolking ved bruk av `elm/time`:
`elm/time` tilbyr `Time`-modulen, som lar deg jobbe med tidsstempler i stedet for lesbare datoer for mennesker. Selv om den ikke direkte tolker datoer fra strenger, kan du konvertere en ISO 8601-streng til et POSIX-tidsstempel, som du deretter kan arbeide med.

```elm
import Time exposing (Posix)

-- Anta at du har en ISO 8601 dato-streng
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- Konverter den til et POSIX-tidsstempel (denne funksjonen returnerer et `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Eksempel på utdata: Ok <posix tid verdi>
```

### Tolking ved bruk av `justinmimbs/date`:
For mer intrikat tolking, som håndtering av ikke-ISO-formater, er `justinmimbs/date`-biblioteket et flott valg. Her er hvordan du kan bruke det til å tolke en tilpasset datostreng:

1. Sørg for at du har biblioteket installert:

```shell
elm install justinmimbs/date
```

2. Bruk funksjonen `Date.fromString` for å tolke tilpassede datoformater:

```elm
import Date
import Result exposing (Result(..))

-- La oss si at du har et tilpasset dato-strengformat `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Funksjon for å tolke det tilpassede formatet
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Eksempel på bruk
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Eksempel på utdata: Ok (Date.fromCalendarDate 2023 Jan 1)
```

I disse eksemplene innkapsler `Result`-typen enten en vellykket tolking som gir en dato (`Ok`) eller en feil (`Err`), noe som muliggjør robust feilhåndtering i Elm-applikasjonene dine.
