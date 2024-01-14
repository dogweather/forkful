---
title:                "Elm: Å få den nåværende datoen"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Hvorfor
Et spørsmål som mange nybegynnere innenfor programmering ofte stiller er, "hvorfor trenger vi å få dagens dato i en applikasjon?" Det er et godt spørsmål, og i denne bloggposten skal vi utforske hvorfor å få dagens dato er en viktig funksjon innenfor Elm-programmering.

##Hvordan
Kodeeksempel:

```Elm
import Time exposing (Posix)
import Date exposing (Date, DateType)

getDate : Posix -> Date
getDate time =
    Time.fromPosix time |> Date.fromTime

getDateString : Date -> String
getDateString date =
    case Date.toParts date of
        Just parts ->
            debug (toString parts.year ++ "-" ++ toString parts.month ++ "-" ++ toString parts.day)

```

Output:
2019-7-2

For å få dagens dato i Elm, må vi først importere modulene Time og Date. Deretter kan vi bruke funksjonen `fromPosix` fra Time-modulen for å konvertere tiden fra et `Posix`-format til et `Time`-format. Deretter bruker vi Date-modulen for å konvertere `Time`-formatet til en `Date`-format. Når vi har fått dagens dato i `Date`-format, kan vi bruke funksjonen `toParts` for å få en liste over alle delene av datoen, som år, måned og dag. Til slutt bruker vi `toString` for å konvertere disse delene til `String`-format og setter de sammen for å få en lesbar dato.

##Dykk Dypere
Vi kan også bruke funksjonen `getFullYear` fra `Time`-modulen for å få dagens årstall direkte, og `getMonth` for å få måneden som et tall. Dette kan være nyttig hvis vi bare trenger en del av datoen i applikasjonen vår.

En annen viktig ting å merke seg er at datoen som blir returnert av disse funksjonene er basert på UTC-tidszonen. Hvis applikasjonen vår skal brukes i et bestemt land eller region, må vi sørge for å konvertere datoen til den lokale tidszonen. Dette kan gjøres ved å bruke funksjonen `toZone` fra `Time`-modulen. 

##Se Også
- Offisiell Elm dokumentasjon for Time-modulen: https://package.elm-lang.org/packages/elm/time/latest/Time
- Offisiell Elm dokumentasjon for Date-modulen: https://package.elm-lang.org/packages/elm/time/latest/Date
- Elm-tutorial om å få og håndtere datoer: https://frontend.center/elm-tutorial/dates/
- Video-tutorial om å bruke Time-modulen i Elm: https://www.youtube.com/watch?v=gYqFxHcBdas