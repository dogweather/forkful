---
title:                "Haskell: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Varför

Att konvertera en sträng till små bokstäver är en vanlig uppgift inom programmering. Detta kan vara användbart för att jämföra strängar på ett enhetligt sätt eller för att skapa en sökfunktion som ignorerar bokstavskapital.

##Så här gör du

För att konvertera en sträng till små bokstäver i Haskell, använder vi den inbyggda funktionen `map` tillsammans med funktionen `toLower` från modulen `Data.Char`. Kodexemplet nedan visar hur detta kan göras:

```Haskell
import Data.Char (toLower)

strToLower :: String -> String
strToLower = map toLower
```

I detta exempel definierar vi en funktion `strToLower` som tar en sträng som argument och använder `map` för att applicera `toLower` på varje tecken i strängen. För att använda denna funktion i vår kod, kan vi sedan helt enkelt skriva `strToLower "EXEMPELSTRÄNG"` och få ut resultatet `"exempelsträng"`.

##Djupdykning

För att förstå hur funktionen `map` fungerar i samband med `toLower`, måste vi först förstå datatypen `String` i Haskell. I Haskell är `String` en samling av `Char`-värden, vilket betyder att varje element eller bokstav i en sträng är representerad av en `Char`.

Funktionen `map` tar en funktion som första argument och en lista som andra argument. Den applicerar sedan funktionen på varje element i listan och returnerar en ny lista med de nya värdena. I vårt fall är funktionen som vi vill applicera `toLower` och listan är vår `String`. Detta innebär att `map toLower "EXEMPELSTRÄNG"` resulterar i en ny lista med bokstäverna i strängen konverterade till små bokstäver.

##Se även

- [Haskell Data.Char dokumentation](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#v:toLower)
- [Haskell map dokumentation](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:map)