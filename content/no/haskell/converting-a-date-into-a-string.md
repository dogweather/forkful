---
title:                "Haskell: Konvertering av dato til streng."
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere kan enes om at datoer kan være vanskelige å håndtere. Det er ikke alltid like enkelt å arbeide med dem i programvare som forventer å få en tekstverdi som input. Derfor kan det være nyttig å vite hvordan man kan konvertere datoer til tekst, noe som kan gjøres med Haskell-programmering.

## Hvordan

For å konvertere en dato til en streng i Haskell, kan man bruke funksjonen `show`. Denne funksjonen tar inn en datoverdi og returnerer en strengrepresentasjon av den. La oss ta en nærmere titt på hvordan dette kan gjøres i praksis:

```Haskell
import Data.Time (UTCTime, formatTime, defaultTimeLocale)

getCurrentTime :: IO UTCTime
curTime = getCurrentTime

dateToStr :: UTCTime -> String
dateToStr time = formatTime defaultTimeLocale "%d.%m.%Y" time 

main :: IO ()
main = do
  time <- curTime
  let str = dateToStr time
  putStrLn str
```

I dette eksempelet bruker vi `Data.Time` -modulen for å arbeide med datoer. Vi lager en funksjon `getCurrentTime` som henter dagens dato, og en annen funksjon `dateToStr` som tar inn en `UTCTime`-verdi og returnerer en streng i ønsket format. Deretter bruker vi `putStrLn` for å skrive ut resultatet til konsollen.

Når vi kjører programmet, vil utskriften se slik ut:

```
11.08.2021
```

På denne måten kan man enkelt konvertere datoer til tekst i Haskell.

## Dypdykk

For å forstå litt mer om hvordan konverteringen fra dato til streng fungerer, kan det være nyttig å se på argumentene som `formatTime` -funksjonen tar inn. Først tar den inn en `TimeLocale` -verdi, som definerer formatet på datoen. Deretter tar den inn en `UTCTime` -verdi og returnerer en streng. Denne strengen kan også tilpasses ved å legge til forskjellige formatteringsstrenger i `formatTime` -funksjonen.

Når man arbeider med datoer i Haskell, er det også viktig å være klar over at modulen `Data.Time` krever at man impoterer modulen `Data.Time.Calendar` for å gjøre datoer tilgjengelige.

## Se Også

- [Haskell.org](https://www.haskell.org/)
- [Data.Time modulen](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [FormatString referanse](https://hackage.haskell.org/package/time/docs/System-Locale.html#v:formatString)