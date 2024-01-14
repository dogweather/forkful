---
title:    "Haskell: Konvertere en streng til små bokstaver"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten vil vi se på hvordan man kan konvertere en streng til små bokstaver i Haskell. Dette kan være nyttig for å gjøre tekstbehandling enklere, for eksempel når man skal sammenligne to tekster uten å måtte ta hensyn til store og små bokstaver.

## Hvordan gjøre det

Å konvertere en streng til små bokstaver i Haskell er en enkel prosess. Vi kan bruke funksjonen `toLower` fra modulen `Data.Char` for å gjøre dette. La oss se på et eksempel:

```Haskell
import Data.Char

konverterTekst :: String -> String
konverterTekst tekst = map toLower tekst
```
Her bruker vi `map`-funksjonen til å bruke `toLower` på hver bokstav i teksten vår. Resultatet vil være en ny streng med alle bokstavene i små bokstaver.

La oss prøve å kjøre koden med en tekst og se på resultatet:

```Haskell
*Main> konverterTekst "HeLLo Haskell"
"hello haskell"
```

Som du kan se, blir alle bokstavene i teksten vår omgjort til små bokstaver, og vi kan bruke denne funksjonen på alle typer strenger.

## Dykk dypere

Som nevnt tidligere, bruker vi funksjonen `toLower` fra modulen `Data.Char` for å konvertere bokstavene til små bokstaver. Denne funksjonen tar inn en bokstav som et argument og returnerer tilsvarende bokstav i små bokstaver.

Det som er spesielt med denne funksjonen er at den også konverterer bokstaver fra språk som bruker andre alfabet, som for eksempel det greske alfabetet. Dette betyr at uansett hvilket språk eller tegnsett teksten din er på, vil denne funksjonen fungere som den skal.

## Se også

- [Haskell-dokumentasjon for Data.Char-modulen](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)
- [Tutorial om strenger og teksthåndtering i Haskell](https://learnyouahaskell.com/starting-out#reading-and-writing-files)