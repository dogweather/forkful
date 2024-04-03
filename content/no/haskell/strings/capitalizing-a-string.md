---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:25.995963-07:00
description: "Det \xE5 kapitalisere en streng inneb\xE6rer \xE5 transformere den f\xF8\
  rste bokstaven i en gitt streng til stor bokstav mens resten av bokstavene forblir\
  \ sm\xE5.\u2026"
lastmod: '2024-03-13T22:44:40.823613-06:00'
model: gpt-4-0125-preview
summary: "Det \xE5 kapitalisere en streng inneb\xE6rer \xE5 transformere den f\xF8\
  rste bokstaven i en gitt streng til stor bokstav mens resten av bokstavene forblir\
  \ sm\xE5."
title: Sette stor bokstav i en streng
weight: 2
---

## Hvordan:
I Haskell kan du kapitalisere en streng ved å bruke standardbiblioteket uten å trenge noen tredjepartsbiblioteker.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Eksempel på bruk:
main = putStrLn $ capitalize "hello world"
```

Utdata:
```
Hello world
```

For mer komplekse scenarioer eller for enklere bruk, kan du ønske å bruke et tredjepartsbibliotek som `text`, som er populært for effektiv strengmanipulering i Haskell.

Først må du legge til `text` i prosjektets avhengigheter. Deretter kan du bruke funksjonene dens til å kapitalisere en streng som følger:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Eksempel på bruk med tekstbiblioteket:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Utdata:
```
Hello world
```

Begge disse eksemplene viser enkle men effektive måter å kapitalisere en streng på i Haskell, med eller uten tredjepartsbiblioteker.
