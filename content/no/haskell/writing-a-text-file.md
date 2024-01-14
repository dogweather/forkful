---
title:                "Haskell: Å skrive en tekstfil"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har programmert i Haskell, har du kanskje lagt merke til at de fleste programmer starter med å importere moduler og deretter definere funksjoner og variabler. Men hvorfor bry seg med å skrive alt i et tekstfilformat? Det er tross alt bare en annen måte å skrive koden på. I denne bloggposten skal vi utforske hvorfor det er nyttig å skrive en Haskell-programkode i et tekstfilformat.

# Hvordan

Det første vi trenger å gjøre er å opprette en ny fil med "hs" utvidelse, for eksempel "main.hs". Dette er en konvensjonell måte å navngi en Haskell-programkodefil på, selv om du kan kalle den hva du vil. Deretter kan vi begynne å skrive koden vår ved å definere modulnavnet, importere eventuelle nødvendige moduler og skrive funksjoner og variabler. La oss se på et eksempel:

```Haskell
module Main where

import Data.List

faktoriser :: Int -> [Int]
faktoriser tallet = findDelere tallet [2..tallet]
  where findDelere _ []         = []
        findDelere tall (x:mulige)
          | tall `mod` x == 0 = x : findDelere (tall `div` x) mulige
          | otherwise         = findDelere tall mulige

main = do
  putStrLn "Skriv inn et tall: "
  input <- getLine
  let tall = read input :: Int
  putStrLn (show (faktoriser tall))
```

I dette eksempelet har vi definert en funksjon "faktoriser" som finner alle faktorene til et gitt tall. Vi har også brukt funksjonen "getLine" for å lese inn et tall fra brukeren og deretter brukt "read" for å konvertere det til en Integer. Til slutt skriver vi ut resultatet ved hjelp av "show" funksjonen. Når koden er lagret i en fil, kan vi enkelt kjøre den ved å bruke kommandoen:

```bash
runhaskell main.hs
```

# Dypdykk

Nå spør du deg kanskje hvorfor vi bryr oss med å skrive koden i en fil når vi likevel må bruke en kommandolinjekommando for å kjøre den. Svaret er enkelt, det handler om organisering og gjenbruk av kode. Ved å ha koden din i separate filer, kan du enkelt organisere og strukturere koden din på en bedre måte. Du kan også gjenbruke kode fra en fil i en annen fil ved å importere moduler. Dette kan være svært nyttig når du jobber med større og mer komplekse programmer.

# Se også

- [Haskell dokumentasjon](https://www.haskell.org/documentation/#books)
- [Haskell tutorial](https://wiki.haskell.org/Tutorials)
- [Haskell programmeringsmiljø](https://www.haskell.org/platform/)