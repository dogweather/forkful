---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Gjør strenger om til små bokstaver i Haskell

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver betyr å endre alle store bokstaver i strengen til tilsvarende små bokstaver. Programmerere gjør dette ofte for å unngå å forskjellsbehandle data basert på bokstavstørrelse.

## Hvordan:
Her bruker vi Haskell-funksjonen `toLower` fra  `Data.Char`-modulen til å konvertere en streng til små bokstaver.

```Haskell
import Data.Char (toLower)

toLowerString :: String -> String
toLowerString = map toLower

main = print $ toLowerString "HELLO, WORLD!"
```

Kjører du dette programmet, vil oppførselen bli som følger:

```
"hhello, world!"
```

## Dykker i Dybden
Det å omforme store til små bokstaver i programmering har en lang historie. I tidlige ASCII-kode systemer var det separasjon mellom store og små bokstaver. Konvertering mellom de to ble ofte gjort ved å bruke litt-manipulasjon.

I Haskell bruker vi `toLower`-funksjonen, som håndterer dette for oss. Men det er verdt å nevne at Haskell er en unicode-bevisst språk. Derfor vil `toLower` fungere korrekt selv for ikke-ASCII-tegn. For eksempel, til og med spesielle norske bokstaver som 'Å', 'Æ' og 'Ø' vil bli korrekt konvertert til 'å', 'æ' og 'ø' respektivt.

Alternativt, om man ønsker å lage en egen funksjon for konvertering, så kan man benytte seg av Haskell's støtte for mønstermatching:

```Haskell
lowercase :: Char -> Char
lowercase c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c
```
Men merk at denne funksjonen kun vil fungere korrekt for ASCII-tegn.

## Se Også:
1. Haskell.org's bibliotekside for `Data.Char`: <https://hackage.haskell.org/package/base/docs/Data-Char.html>
2. Lærebok om generell strengmanipulasjon i Haskell: <http://learnyouahaskell.com/input-and-output>
3. Haskell's stilguide for gode kodevaner: <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>