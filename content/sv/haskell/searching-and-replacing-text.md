---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Haskell Programmering: Söka och Ersätta text

## Vad & Varför?
Sökning och ersättning av text är en process att hitta specifika strängar i koden och ersätta dem med något annat. Det är viktigt för programmerare att automatisera ändringar och reducera mänskliga fel.

## Hur man gör:
För att söka och ersätta text i Haskell, behöver vi använda biblioteket `Data.Text` som innehåller en funktion som heter `replace`. Nedanstående är ett exempel:

```Haskell
import Data.Text (replace, pack, unpack)

main :: IO ()
main = do
   let originalText = pack "Jag älskar att programmera i Haskell"
   let replacer = replace (pack "älskar") (pack "hatar")
   putStrLn . unpack $ replacer originalText
```

När du kör detta program, kommer utdata vara "Jag hatar att programmera i Haskell".

## Djupdykning
Sök och ersätta funktioner har en lång historia inom programmering. De började med tidiga kodredigeringsverktyg som sedan utvidgades till programmeringsspråk.

Alternativ till den tidigare metoden inkluderar användning av reguljära uttryck eller 'regex', som kan hantera mer komplexa sökningar och ersättningar men kan vara svårare att läsa.

Detaljerna för hur `Data.Text.replace` fungerar är intressanta - den fungerar genom att konvertera den ursprungliga strängen och den sträng du vill byta ut till listor av tecken (eller `Text` objekt). Sedan använder den listfunktioner för att göra bytet.

## Se även
För mer information om hur man jobbar med text i Haskell, kolla in:

- The `Data.Text` dokumentation: [https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- En bra genomgång av Haskell listor: [http://learnyouahaskell.com/starting-out#lists-and-tuples](http://learnyouahaskell.com/starting-out#lists-and-tuples)
- För mer om att använda regex i Haskell, besök: [http://www.serpentine.com/blog/2007/02/27/a-haskell-regular-expression-tutorial/](http://www.serpentine.com/blog/2007/02/27/a-haskell-regular-expression-tutorial/)