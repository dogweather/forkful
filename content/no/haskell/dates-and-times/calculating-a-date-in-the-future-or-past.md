---
date: 2024-01-20 17:31:08.714034-07:00
description: "\xC5 regne ut en dato i fremtiden eller fortiden handler om \xE5 finne\
  \ en bestemt dag f\xF8r eller etter en kjent dato. Programmerere gj\xF8r dette for\
  \ \xE5 h\xE5ndtere\u2026"
lastmod: '2024-03-13T22:44:40.856018-06:00'
model: gpt-4-1106-preview
summary: "\xC5 regne ut en dato i fremtiden eller fortiden handler om \xE5 finne en\
  \ bestemt dag f\xF8r eller etter en kjent dato."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hvordan gjøre det:
Haskell bruker biblioteket `Data.Time` for datooperasjoner. Her er et eksempel på hvordan du finner en fremtidig dato:

```haskell
import Data.Time

-- Legg til et gitt antall dager til dagens dato og skriv ut den nye datoen
main = do 
    today <- utctDay <$> getCurrentTime
    let futureDate = addDays 10 today
    print futureDate
```

Eksempelutdata vil være avhengig av når koden kjøres. For eksempel:

```
2023-03-30
```

For å finne en dato i fortiden:

```haskell
-- Trekk fra et gitt antall dager fra dagens dato og skriv ut den nye datoen
main = do 
    today <- utctDay <$> getCurrentTime
    let pastDate = addDays (-10) today
    print pastDate
```

## Dypdykk:
Tidligere ble datoer ofte beregnet manuelt, noe som førte til feil og inkonsistens. I Haskell gir `Data.Time` et robust sett med funksjoner for tidsberegning, reduserer feil og forbedrer internasjonaliseringen.

Alternativer til `Data.Time` inkluderer lavnivå biblioteker som `time` og `old-time`, men disse er mindre praktiske og ofte mer feilutsatte.

For implementeringsdetaljer: `addDays` funksjonen tar hensyn til skuddår og andre kalenderanomalier. Den fungerer ved å konvertere en `Day` til et såkalt Modified Julian Date-nummer, endrer tallet og konverterer det tilbake.

## Se Også:
- [Haskell `Data.Time` dokumentasjon](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
