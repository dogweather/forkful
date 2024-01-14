---
title:                "Haskell: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering kommer vi over behovet for å kunne beregne datoer i fremtiden eller fortiden. Dette kan være nyttig for å planlegge fremtidige hendelser eller for å håndtere forskjellige tidsdeler i en applikasjon.

## Hvordan Gjøre Det

For å kunne beregne datoer i Haskell, trenger vi å bruke noen innebygde funksjoner og typer. La oss se på et eksempel der vi ønsker å beregne datoen 20 dager fra nå:

```Haskell
import Data.Time

main = do
    nå <- getCurrentTime
    la oss si = addDays 20 nå
    print (formatTime defaultTimeLocale "%d/%m/%Y" la oss si)
```

Output: 02/03/2021

Vi begynner ved å importere `Data.Time` biblioteket som inneholder funksjoner og typer relatert til dato og tid. Deretter får vi gjeldene tidspunkt ved hjelp av `getCurrentTime` funksjonen og lagrer det i `nå` variabelen. Videre bruker vi `addDays` funksjonen for å legge til 20 dager til `nå` og lagrer resultatet i `la oss si` variabelen. Til slutt bruker vi `formatTime` funksjonen for å formatere datoen slik at den vises som dag/måned/år.

Et annet eksempel, denne gangen hvor vi vil trekke fra 10 dager fra dagens dato:

```Haskell
...
la oss si = addDays (-10) nå
print (formatTime defaultTimeLocale "%d/%m/%Y" la oss si)
```

Output: 11/02/2021

Vi bruker den samme tilnærmingen, men legg merke til at vi bruker `addDays (-10)` for å trekke fra dager i stedet for å legge til.

## Dypdykk

Nå som vi har sett hvordan vi kan beregne datoer i Haskell, la oss se på noen av de tilgjengelige funksjonene og typer som kan være nyttige for oss:

- `getCurrentTime` - denne funksjonen gir oss gjeldende tidspunkt som en `UTCTime` type.
- `addDays` - denne funksjonen legger til eller trekker fra et gitt antall dager fra et gitt tidspunkt.
- `addMonths` - denne funksjonen legger til eller trekker fra et gitt antall måneder fra et gitt tidspunkt.
- `addYears` - denne funksjonen legger til eller trekker fra et gitt antall år fra et gitt tidspunkt.
- `formatTime` - denne funksjonen formaterer et gitt tidspunkt til ønsket format ved å bruke en spesifisert tidssone og språk.
- `Day` - dette er en type som representerer en dag.
- `diffDays` - denne funksjonen beregner antall dager mellom to gitt datoer.

Det finnes også mange andre funksjoner og typer som kan være nyttige for dato/tid manipulasjon i Haskell. Vi oppfordrer deg til å utforske disse og se hva annet du kan gjøre med dem!

## Se Også

- [Haskell Dokumentasjon om Dato og Tid](https://downloads.haskell.org/~ghc/8.10.1/docs/html/libraries/time-1.9.3/Data-Time.html)
- [Haskell Wikibook - Date and Time](https://en.wikibooks.org/wiki/Haskell/Dates_and_times)
- [Haskell.org discusjonsforum](https://wiki.haskell.org/Reddit#Community)