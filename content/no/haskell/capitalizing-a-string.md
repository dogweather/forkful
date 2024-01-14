---
title:                "Haskell: Stor bokstaving av en streng"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å kapitalisere en streng er en vanlig operasjon i mange programmeringsspråk, inkludert Haskell. Dette handler om å gjøre den første bokstaven i en streng til en stor bokstav. Dette kan være nyttig når man ønsker å presentere data på en ryddig og konsistent måte, eller når man trenger å sammenligne to strenger.

# Hvordan

```Haskell
import Data.Char -- importerer Data.Char biblioteket for å få tilgang til funksjoner som hjelper oss med å kapitalisere strenger
capitalize :: String -> String -- funksjonen tar en streng som input og returnerer en streng
capitalize [] = [] -- hvis den tomme strengen blir gitt som input, returnerer vi den tomme strengen
capitalize (x:xs) = toUpper x : xs -- vi bruker funksjonen toUpper fra Data.Char biblioteket til å gjøre den første bokstaven i strengen til en stor bokstav, og legger den tilbake til resten av strengen

-- eksempel på bruk av funksjonen
capitalize "haskell" -- "Haskell"
capitalize "hello world" -- "Hello world"
capitalize "123abc" -- "123abc" siden tall ikke blir påvirket av funksjonen
```

# Dypdykk

Selv om det kan virke som en enkel operasjon, er det noen ting å merke seg når det kommer til å kapitalisere strenger i Haskell. For det første, kan strenger i Haskell bestå av Unicode-tegn, og ikke bare ASCII-tegn. Derfor må vi være forsiktige når vi bruker funksjoner som `toUpper` for å sikre at alle tegn i strengen blir behandlet riktig.

I tillegg er det viktig å være oppmerksom på at funksjonen `capitalize` som er vist over bare tar hensyn til den første bokstaven i en streng. Hvis man ønsker å kapitalisere alle bokstaver i en streng, må man bruke en annen tilnærming.

En annen nyttig ting å vite er at funksjonen `toUpper` returnerer en `Char` type, ikke en `String` type. Derfor må man bruke funksjoner som `map` for å bruke `toUpper` på alle tegn i en streng.

# Se også

- [Haskell Standard Libraries](http://www.haskell.org/onlinereport/standard-prelude.html)
- [Unicode og Haskell](https://wiki.haskell.org/Unicode)
- [Stack Overflow: How to capitalize a string in Haskell?](https://stackoverflow.com/questions/13402033/how-to-capitalize-first-letter-of-a-string-in-haskell)