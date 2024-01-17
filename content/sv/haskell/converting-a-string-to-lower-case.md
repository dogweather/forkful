---
title:                "Omvandla en sträng till gemener"
html_title:           "Haskell: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Konvertering av en sträng till gemener är en vanlig åtgärd i programmering. Det innebär att alla bokstäver i en sträng ändras till små bokstäver. Detta kan vara användbart för att göra strängar jämförbara eller för att uppnå en enhetlig formatering. 

# Hur man gör:
```Haskell
import Data.Char (toLower)
toLower "HEJ" -- ger "hej"
toLower "Haskell" -- ger "haskell"
```

# Djupdykning:
Konverteringen av strängar till gemener har varit en funktionelliteten som funnits sedan tidiga språk som C och UNIX shell. Det finns alternativa sätt att åstadkomma detta, en vanlig metod är att använda sig av ASCII-värden för bokstäverna och göra en omvandling. I Haskell används istället den inbyggda funktionen `toLower` från modulen `Data.Char` som effektivt hanterar olika språk och specialtecken.

# Se även:
- [Haskell Dokumentation](https://www.haskell.org/ghc/docs/latest/html/libraries/base-4.9.1.0/Data-Char.html#v:toLower)
- [Wikipedia: Case Sensitivity](https://en.wikipedia.org/wiki/Case_sensitivity)
- [Coderbyte: Convert string to lower case](https://www.coderbyte.com/algorithm/convert-string-to-lowercase)