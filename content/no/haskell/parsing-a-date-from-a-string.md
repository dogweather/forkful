---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av en dato fra en streng er prosessen med å lese en dato skrevet i tekstformat og omdanne den til en datotype som forstås av programmet. Programmører gjør dette for å jobbe mer effektivt med datoer og tider.

## Hvordan:

Her er et eksempel på Hvordan parse en dato fra en streng i Haskell.

```Haskell
import Data.Time

readDate :: String -> Maybe Day
readDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```
Du kan prøve det slik:

```Haskell
*> readDate "2022-03-01"
Just 2022-03-01
*> readDate "Ikke en dato"
Nothing
```

## Dypdykk

Parsing av dato fra en streng har vært en viktig del av programmering siden begynnelsen, da data ofte blir gitt i menneskelig lesbare formater som strenger, men krever konvertering til mer håndterbare formater for beregning.

Alternativt, kan vi bruke biblioteker som `datetime` eller `time` i Haskell for mer komplekse format.

I Haskell, funksjonen `parseTimeM` er del av `Data.Time.Format` modulen. Det bruker en streng mal ("%Y-%m-%d" i dette tilfellet) for å matche og konvertere datoene.

## Se Også

For mer informasjon, sjekk ut disse lenkene:

- [Data.Time.Format documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Dates_and_times)
- [Stack Overflow: How to convert String to Data.Time in Haskell?](https://stackoverflow.com/questions/3030675/how-to-convert-string-to-data-time-in-haskell)