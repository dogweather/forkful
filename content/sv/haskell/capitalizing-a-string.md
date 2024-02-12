---
title:                "Gör om en sträng till versaler"
aliases:
- sv/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:21.459938-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att förstora en sträng innebär att omvandla den första bokstaven i en given sträng till versal medan resten av bokstäverna förblir gemener. Programmerare gör detta för att formatera utdata, följa grammatisk korrekthet i texter, eller förbättra läsbarheten av genererade data.

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
