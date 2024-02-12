---
title:                "Extrahera delsträngar"
aliases:
- sv/haskell/extracting-substrings.md
date:                  2024-01-20T17:45:43.674143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Att extrahera delsträngar innebär att man plockar ut specifika delar av en textsträng. Programmerare gör detta för att manipulera och analysera data, eller för att enkelt kunna skapa specifika textbaserade format.

## How to:
I Haskell använder du ofta standardbibliotekets funktioner för att hantera strängar. Här är några exempel på hur man extraherar delsträngar:

```Haskell
import Data.Text (Text)
import qualified Data.Text as T

-- Exempel: Extrahera en delsträng med `take` och `drop`
extractSubstr :: Text -> Int -> Int -> Text
extractSubstr str start len = T.take len . T.drop start $ str

-- Användning:
main :: IO ()
main = do
    let text = T.pack "Hej, världen!"
    putStrLn . T.unpack $ extractSubstr text 5 7  -- Output: "världen"
```

## Deep Dive
I Haskell använder man ofta `Data.Text` för strängmanipulation, eftersom det är mer effektivt än `String`. Historiskt sett användes `String` (en lista av tecken), men det var långsamt för större textmängder.

Det finns andra sätt att hantera sub-strings också. Modulen `Data.Text` tillhandahåller `take`, `drop`, och `splitAt` bland andra, som låter dig skära och sätta ihop strängar som du vill.

Implementationsmässigt använder `Data.Text` en array av Unicode-tecken (UTF-16 kodade) vilket gör det snabbare än `String` (som är en länkad lista av tecken).

## See Also
Mer om `Data.Text`: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html

För grundläggande Haskell-strängmanipulation, se LYAH: http://learnyouahaskell.com/starting-out#strings
