---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:21.459938-07:00
description: "Hur man g\xF6r: I Haskell kan du f\xF6rstora en str\xE4ng med hj\xE4\
  lp av standardbiblioteket utan att beh\xF6va n\xE5gra tredjepartsbibliotek."
lastmod: '2024-03-13T22:44:37.938986-06:00'
model: gpt-4-0125-preview
summary: "I Haskell kan du f\xF6rstora en str\xE4ng med hj\xE4lp av standardbiblioteket\
  \ utan att beh\xF6va n\xE5gra tredjepartsbibliotek."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:
I Haskell kan du förstora en sträng med hjälp av standardbiblioteket utan att behöva några tredjepartsbibliotek.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Exempel på användning:
main = putStrLn $ capitalize "hello world"
```

Utdata:
```
Hello world
```

För mer komplexa scenarier eller för enkelhetens skull kanske du vill använda ett tredjepartsbibliotek som `text`, som är populärt för effektiv strängmanipulering i Haskell.

Först måste du lägga till `text` i ditt projekts beroenden. Sedan kan du använda dess funktioner för att förstora en sträng som följer:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Exempel på användning med textbiblioteket:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Utdata:
```
Hello world
```

Båda dessa exempel demonstrerar enkla men effektiva sätt att förstora en sträng i Haskell, med eller utan tredjepartsbibliotek.
