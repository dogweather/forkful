---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "Haskell: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Visste du at du kan bruke Haskell til å enkelt beregne en dato i fortiden eller fremtiden? Uansett om du er glemsom og trenger å finne ut hvilken dag det var for to uker siden, eller om du ønsker å planlegge en spesiell begivenhet i fremtiden, kan du bruke Haskell til å gjøre det på en enkel og effektiv måte.

## Hvordan
For å kunne beregne en dato i fremtiden eller fortiden trenger du to viktige funksjoner: `addDays` og `fromGregorian`. `addDays` lar deg legge til et bestemt antall dager til en dato, mens `fromGregorian` lar deg konvertere en dato fra å være på formen "år, måned, dag" til en `Day` datatype i Haskell.

```Haskell
import Data.Time.Calendar

-- Dato 7. juli 2021
let currentDay = fromGregorian 2021 7 7

-- Legg til 10 dager til nåværende dato
let futureDay = addDays 10 currentDay

-- Legg til 2 uker og 3 dager til nåværende dato
let futureDay2 = addDays 17 currentDay

-- Skriver ut fremtidige datoer
print currentDay --2021-07-07
print futureDay --2021-07-17
print futureDay2 --2021-07-24
```

## Dypdykk
I eksemplene ovenfor la vi til et positivt antall dager til nåværende dato for å få en fremtidig dato. Men hva om vi ønsker å få en dato i fortiden? Det er enkelt, vi bruker bare et negativt antall dager i `addDays` funksjonen.

```Haskell
-- Dato 7. juli 2021
let currentDay = fromGregorian 2021 7 7

-- Trekk fra 5 dager for å få en dato i fortiden
let pastDay = addDays (-5) currentDay

-- Skriver ut dato i fortiden
print pastDay --2021-07-02
```

Det er også mulig å legge til eller trekke fra et bestemt antall uker eller måneder ved å konvertere det til dager. For eksempel vil en uke tilsvare 7 dager og en måned vil variere avhengig av hvilken måned du befinner deg i.

## Se også
- https://devblogs.microsoft.com/haskell/calculating-dates-in-haskell/