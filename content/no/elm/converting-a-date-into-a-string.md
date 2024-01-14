---
title:                "Elm: Konvertere dato til en streng"
simple_title:         "Konvertere dato til en streng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en dato til en tekststreng kan være nyttig i ulike scenarioer. Dette kan for eksempel være hvis du vil presentere datoer på en mer lesbar måte eller hvis du vil inkludere datoer i tekstutskrifter eller e-postmeldinger.

## Hvordan

For å konvertere en dato til en streng i Elm, kan du bruke funksjonen `toString` fra `Date` modulen. Her er et eksempel på hvordan dette kan gjøres:

```Elm
import Date exposing (Date, Day, Month, Year, toString)
import Time exposing (hour, minute, second)

-- Opprett en dato
date : Date
date =
    Date.fromCalendarDate 2021 7 15

-- Konverter datoen til en streng
dateString : String
dateString =
    Date.toString "%d.%m.%Y" date

-- Skriv ut resultatet
main : Html msg
main =
    div []
        [ text dateString ] -- Output: 15.07.2021
```

Her bruker vi funksjonen `fromCalendarDate` for å opprette en dato med verdiene for år, måned og dag. Deretter bruker vi `toString` for å konvertere denne datoen til en streng ved hjelp av et format.

Formatet i eksempelet over er `"%d.%m.%Y"`, der `day` representerer dagen, `month` måneden og `year` året. Dette formatet vil gi oss en streng som ser ut som `DD.MM.ÅÅÅÅ`. Du kan også bruke andre formater, som for eksempel `"%Y-%m-%d"` for å få en streng på formatet `ÅÅÅÅ-MM-DD`.

## Dypdykk

Det er verdt å merke seg at når du konverterer en dato til en streng, så vil det endre seg avhengig av tidssonen som brukeren din befinner seg i. Dette er fordi datostemplingen blir påvirket av tidssonen, og `toString` funksjonen bruker dette til å bestemme verdien av dagen, måneden og året.

Hvis du er interessert i å lære mer om dette temaet, så anbefaler vi å se nærmere på Time og Date modulene i Elm-dokumentasjonen. Disse modulene inneholder flere nyttige funksjoner for å håndtere datoer og tid.

## Se også

- [Elm-dokumentasjon for Date-modulen](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm-dokumentasjon for Time-modulen](https://package.elm-lang.org/packages/elm/time/latest/Time)