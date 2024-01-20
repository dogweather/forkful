---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Elm: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden innebærer å legge til eller trekke fra et bestemt antall dager, måneder eller år fra en gitt dato. Programmerere gjør dette for å håndtere datorelaterte funksjoner i applikasjoner, som planleggende hendelser, påminnelser, oppgaver og mer.

## Hvordan gjøre det:

Her er et grunnleggende eksempel på Elm for å beregne en fremtidig dato, i dette tilfellet 5 dager framover:

```Elm
import Time

main =
  let 
    today = Time.millisToPosix 1636090328000
    fiveDaysInMilliseconds = 5 * 24 * 60 * 60 * 1000
  in 
    Debug.toString (Time.posixToMillis (Time.adjust (Time.millisToDuration fiveDaysInMilliseconds) today))
```

Output vil være en tidsstempel som representerer datoen 5 dager senere.

## Dyp Dykk:

**Historisk Kontekst:** Dato-beregning er en eldre praksis i programmering, til og med fra før datamaskintiden. Det hjelper oss med å løse mange praktiske problemer i programvareutvikling.

**Alternativer:** Du kan finne flere pakker for å håndtere datoer i Elm, som `elm-date-extra` og `ryannhg/date-format`. De gir mer omfattende håndtering av datoformat, tidssoner, osv.

**Implementasjonsdetaljer:** Standardbiblioteket `Time` i Elm bruker Posix (Unix-tid) for å representere tid, som er antall millisekunder siden 'den første januar 1970 UTC'. For enkelhet har vi brukt en konstant tidsstempel for 'i dag'. I en ekte applikasjon vil du sannsynligvis få den faktiske tiden ved å bruke 'Time.now`.

## Se Også:

- [Offisiell Elm Dokumentasjon for Tid (Time)](https://package.elm-lang.org/packages/elm/time/latest/)