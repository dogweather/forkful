---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Haskell: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en fremtidig eller tidligere dato betyr å finne ut når en dato vil eller har falt på basis av oppgitte dager, måneder eller år. Dette er nyttig i programmering for å håndtere planlegging, hendelsesstyring og tidsbasert logikk i applikasjoner.

## Hvordan:
Vi bruker et innebygd bibliotek 'Data.Time.Calendar' for dette formålet. For eksempel, manifestasjonen:

```Haskell
import Data.Time.Calendar
import Data.Time.Clock

main = do
    let today = fromGregorian 2021 9 1
    print $ addDays 90 today
    print $ addDays (-90) today
```
Utfører følgende:
```
2021-11-30
2021-06-03
```
Disse linjene utskriver en dato 90 dager frem i tid og en dato 90 dager tilbake i tid fra en gitt dato (1. september 2021).

## Dypdykk:
Å beregne en dato i fremtiden eller fortiden har vært en viktig del av programmering siden tidlige dager. En slik beregning er nyttig i mange sammenhenger, som flyreservasjoner, påminnelser og planlægning af aktiviteter.

En alternativ tilnærming ville være å bruke biblioteket 'Data.Time.LocalTime', som gir mer kontroll over lokal tid og tidssoner.

En interessant detalj er at funksjonen 'addDays' usynlig håndterer skuddår, så det er en pålitelig metode for datoaddisjon.

## Se Også:
- [Time Library Documentation](https://hackage.haskell.org/package/time-1.10/docs/Data-Time.html)
- [Haskell Wiki Time Page](https://wiki.haskell.org/Time)
- [Haskell Add Days To Date Example](http://learnyouahaskell.com/input-and-output)