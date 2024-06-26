---
date: 2024-01-20 17:57:52.464949-07:00
description: "Hvordan: I Haskell kan tekst s\xF8kes og erstattes ved hjelp av funksjoner\
  \ fra standardbiblioteket, for eksempel `Data.Text` som h\xE5ndterer strengdata\u2026"
lastmod: '2024-03-13T22:44:40.825803-06:00'
model: gpt-4-1106-preview
summary: "I Haskell kan tekst s\xF8kes og erstattes ved hjelp av funksjoner fra standardbiblioteket,\
  \ for eksempel `Data.Text` som h\xE5ndterer strengdata effektivt."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

## Hvordan:
I Haskell kan tekst søkes og erstattes ved hjelp av funksjoner fra standardbiblioteket, for eksempel `Data.Text` som håndterer strengdata effektivt. 
```Haskell
import Data.Text (replace, pack, unpack)

-- Definerer en enkel erstatningsfunksjon
searchAndReplace :: String -> String -> String -> String
searchAndReplace old new = unpack . replace (pack old) (pack new) . pack

-- Eksempelbruk
main :: IO ()
main = putStrLn $ searchAndReplace "verden" "Norge" "Hallo, verden!"

-- Forventet resultat
-- "Hallo, Norge!"
```

## Dypdykk
Historisk har tekstmanipulasjon vært en kjernefunksjon i programmering. Funksjoner som `sed` i Unix har gjort dette lenge. I Haskell, som er funksjonell, brukes ofte biblioteker som `Data.Text` for effektivitet, siden standard `String` kan være langsom for store datamengder. Et alternativ er `regex-tdfa`-pakken for komplekse søkemønstre. Implementasjonen bruker lazy evaluation, noe som betyr at beregninger blir utsatt til resultatet er nødvendig.

## Se Også
- [Haskell Documentation for `Data.Text`](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- [Haskell Wiki om Regular expressions](https://wiki.haskell.org/Regular_expressions)
- [Real World Haskell, kapittel om strenger og filer](http://book.realworldhaskell.org/read/io.html)
