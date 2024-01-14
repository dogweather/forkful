---
title:    "Haskell: Omvandla en sträng till små bokstäver"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) är en grundläggande uppgift inom programmering och kan vara användbar för att jämföra olika strängar eller för att formatera text på ett enhetligt sätt.

## Så här gör du

För att konvertera en sträng till gemener i Haskell används funktionen `toLower` från standardbiblioteket Data.Char. Funktionen tar en Char (tecken) som argument och returnerar tecknet i gemener.

```Haskell
import Data.Char (toLower)

convertToLower :: String -> String
convertToLower str = map toLower str

-- Exempel: Konvertera strängen "Haskell är ett funktionellt programmeringsspråk"
-- till gemener.
convertToLower "Haskell är ett funktionellt programmeringsspråk" 
-- Output: "haskell är ett funktionellt programmeringsspråk"
```

För att konvertera en hel sträng till gemener kan vi använda oss av funktionen `map` som tar en funktion och en lista som argument och applicerar funktionen på varje element i listan. I vårt fall är funktionen `toLower` som vi applicerar på varje tecken i strängen.

Det är viktigt att notera att funktionen `toLower` bara konverterar bokstäverna i strängen och lämnar andra tecken (som siffror eller specialtecken) oförändrade. Om du vill konvertera hela strängen, inklusive andra tecken, kan du först använda `map` för att konvertera tecknen till bokstäver och sedan använda `toLower`.

## Djupdyk

I de flesta programmeringspråk är det möjligt att konvertera en sträng till gemener med en inbyggd funktion. I Haskell är denna funktion `toLower` från standardbiblioteket Data.Char. Funktionen används ofta tillsammans med andra funktioner som `map` eller `filter` för att manipulera text på olika sätt.

Det är också värt att nämna att konvertering till gemener kan variera beroende på vilket språk eller alfabet som används. I Haskell finns det också en funktion `toLower` som tar två argument, det första är språket och det andra är tecknet som ska konverteras. Detta är särskilt användbart om man arbetar med flerspråkiga texter.

See Also:

- [Learn You a Haskell for Great Good!: Print lady her letters](http://learnyouahaskell.com/modules#data-char)
- [Haskell Language Report: Character and String Literals](https://www.haskell.org/onlinereport/basic.html#character-and-string-literals)
- [Hackage: Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)