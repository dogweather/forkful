---
date: 2024-01-20 17:36:29.612545-07:00
description: "How to: (Hvordan:) Elm gir ikke innebygd datoformatering, s\xE5 vi bruker\
  \ ofte `justinmimbs/date` pakken. Her er et grunnleggende eksempel."
lastmod: '2024-04-05T21:53:41.699270-06:00'
model: gpt-4-1106-preview
summary: "(Hvordan:) Elm gir ikke innebygd datoformatering, s\xE5 vi bruker ofte `justinmimbs/date`\
  \ pakken."
title: Konvertere en dato til en streng
weight: 28
---

## How to: (Hvordan:)
Elm gir ikke innebygd datoformatering, så vi bruker ofte `justinmimbs/date` pakken. Her er et grunnleggende eksempel:

```Elm
import Date
import Date.Extra.Format as DateFormat

convertDateToString : Date.Posix -> String
convertDateToString date =
    DateFormat.format "dd.MM.yyyy" date

-- Bruk
let
    posixDate = Date.fromTime 1609459200000  -- 1. januar 2021
in
convertDateToString posixDate  -- "01.01.2021"
```

## Deep Dive (Dypdykk)
Historisk sett har Elm's kjernebiblioteker hatt begrenset funksjonalitet for håndtering av datoer. `justinmimbs/date` pakken fyller dette gapet ved å tilby enkel og fleksibel datoformatering. Alternativer inkluderer `elm/time` for grunnleggende operaasjoner og `ryannhg/date-format` for mer kompleksitet. Detaljer å huske på er tidssoner og lokaliseringsutfordringer som er håndtert i pakken gjennom `Date.Posix`-typen som representerer tidspunkt i UTC.

## See Also (Se Også)
- Elm Date documentation: https://package.elm-lang.org/packages/elm/time/latest/
- justinmimbs/date package: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Guide for date-format library: https://package.elm-lang.org/packages/ryannhg/date-format/latest/
