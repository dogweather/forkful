---
title:                "Sette stor bokstav i en streng"
aliases:
- no/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:25.995963-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sette stor bokstav i en streng"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Det å kapitalisere en streng innebærer å transformere den første bokstaven i en gitt streng til stor bokstav mens resten av bokstavene forblir små. Programmerere gjør dette for å formatere utdata, overholde grammatisk korrekthet i tekster eller forbedre lesbarheten til genererte data.

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
