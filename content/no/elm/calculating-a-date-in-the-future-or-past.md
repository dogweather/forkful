---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "Elm: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
I denne artikkelen vil vi utforske hvordan du kan bruke Elm til å beregne en dato i fremtiden eller fortiden. Dette kan være nyttig når du for eksempel trenger å lage en datoinput for en kalender eller planlegge hendelser.

## Hvordan
For å beregne en dato i fremtiden eller fortiden i Elm trenger vi å bruke en kombinasjon av funksjoner fra standardbiblioteket. La oss ta en titt på et eksempel:

```Elm
import Date exposing (..)
import Date.Extra exposing (..)

today = fromIsoString "2021-08-15"
tenDaysFromNow = addDays 10 today
threeMonthsAgo = subMonths 3 today

--Output:
--tenDaysFromNow = August 25, 2021
--threeMonthsAgo = May 15, 2021
```

Vi begynner med å importere to moduler, `Date` og `Date.Extra`, som inneholder nyttige funksjoner for arbeid med datoer. Deretter definerer vi dagens dato ved å bruke `fromIsoString` som tar inn en dato i ISO-format (åååå-mm-dd). Videre bruker vi `addDays` og `subMonths` for å beregne en dato 10 dager frem i tid og 3 måneder tilbake i tid. 

## Dykk Dypere
Det er verdt å merke seg at `addDays` og `subMonths` tar inn to argumenter: antall og enhet. Du kan endre enheten til for eksempel uker eller år ved å bruke `Days`, `Weeks`, `Months` eller `Years` som prefiks til tallet. 

En annen nyttig funksjon er `dayOfWeek` som returnerer hvilken ukedag en dato er på. Dette kan være nyttig for å for eksempel lage en kalender eller for å sjekke om en dato faller på en bestemt ukedag. 

Et siste triks er å bruke funksjonen `toString` for å formatere datoer på en spesifikk måte, for eksempel `toString (DayIso Dashes) date` som vil gi et resultat i formatet åååå-mm-dd. 

## Se også
- Elm offisiell dokumentasjon for Date modulen: https://package.elm-lang.org/packages/elm/core/latest/Date
- Elm offisiell dokumentasjon for Date.Extra modulen: https://package.elm-lang.org/packages/elm/core/latest/Date-Extra 
- Enkeltdato kalkulator eksempel på Ellie app: https://ellie-app.com/new