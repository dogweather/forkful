---
aliases:
- /sv/haskell/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:29.226472-07:00
description: "Att konvertera en str\xE4ng till gemener inneb\xE4r att alla versaler\
  \ (stora bokst\xE4ver) i en text omvandlas till gemener (sm\xE5 bokst\xE4ver). Programmerare\
  \ g\xF6r\u2026"
lastmod: 2024-02-18 23:08:51.824851
model: gpt-4-1106-preview
summary: "Att konvertera en str\xE4ng till gemener inneb\xE4r att alla versaler (stora\
  \ bokst\xE4ver) i en text omvandlas till gemener (sm\xE5 bokst\xE4ver). Programmerare\
  \ g\xF6r\u2026"
title: "Konvertera en str\xE4ng till gemener"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att alla versaler (stora bokstäver) i en text omvandlas till gemener (små bokstäver). Programmerare gör detta för att standardisera textdata, exempelvis för att göra sökningar oberoende av bokstävsstorlek.

## Hur gör man:
I Haskell, använder du `Data.Text` för att jobba med textsträngar på ett effektivt sätt. I `Data.Text` finns en funktion `toLower` som konverterar varje tecken i en sträng till gemener. Här är ett enkelt exempel:

```Haskell
import Data.Text (toLower, pack, unpack)

lowercaseStr :: String -> String
lowercaseStr = unpack . toLower . pack

main :: IO ()
main = putStrLn $ lowercaseStr "Hej Världen!"
```

Kör du programmet får du följande resultat:

```
hej världen!
```

## Djupdykning
Att hantera text i programmering har varit relevant så länge som människor har interagerat med datorer. Innan `Data.Text`, använde många `Data.Char` och dess funktion `toLower` för att konvertera enskilda tecken, kombinerat med `map` för att tillämpa konverteringen på varje tecken i en sträng. `Data.Text` blev senare valet för många på grund av bättre prestanda med större textvolym.

Alternativ? Du kan fortfarande använda den äldre metoden:

```Haskell
import Data.Char (toLower)

lowercaseStrOld :: String -> String
lowercaseStrOld = map toLower

-- Och använda den på samma sätt som ovan:
-- main = putStrLn $ lowercaseStrOld "Hej Världen!"
```

Detaljer? `Data.Text` hanterar text som en sekvens av Unicode-tecken, vilket innebär att den kan hantera nästan alla skrivsystem, inte bara ASCII. Vilket är värdefullt för en global värld.

## Se även
* [Data.Text modulen på Hackage](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
* [Data.Char modulen på Hackage](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html)
* [Haskell.org för vidare läsning](https://www.haskell.org/)
* [Unicode standarden](https://unicode.org/standard/standard.html)
