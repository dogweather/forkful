---
date: 2024-01-20 17:32:43.687186-07:00
description: ''
lastmod: '2024-04-05T21:59:53.223243-06:00'
model: gpt-4-1106-preview
summary: ''
title: Sammenlikning av to datoer
weight: 27
---

## How to:
"## Slik gjør du:"

```Elm
import Time
import Date exposing (Date)

-- Definerer to datoer for sammenligning
date1 : Date
date1 = Date.fromParts 2023 March 10

date2 : Date
date2 = Date.fromParts 2023 March 20

-- Sammenligner datoene
compareDates : Date -> Date -> Comparison
compareDates d1 d2 =
    Date.compare d1 d2

-- Se resultatet av sammenligningen
result : String
result =
    case compareDates date1 date2 of
        LT -> "Dato1 kommer før Dato2"
        EQ -> "Datoene er identiske"
        GT -> "Dato1 kommer etter Dato2"

-- Se en utskrift av resultatet
result
-- "Dato1 kommer før Dato2"
```

## Deep Dive
"## Dypdykk"

Historisk sett har dato sammenligning alltid vært en essensiell del av programvareutvikling. Fra planleggingssystemer til å hverdagslige oppgaver. Alternativene inkluderer direkte sammenligning av tidsstempel og bruk av eksterne biblioteker som `elm-time`. Imidlertid, Elm's innebygde `Date` modul håndterer dette ganske greit. Når man sammenligner, konverterer `Date.compare` funksjonen datoene til millisekunder siden epoch for presis sammenligning. Elm gjøre det så enkelt å jobbe med tid uten å bekymre seg for tidssoner, takket være UTC-standard.

## See Also
"## Se Også"

- Elm Date Documentation: [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm Time (alternative third-party library): [https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/)
