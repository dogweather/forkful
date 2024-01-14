---
title:    "Haskell: Konvertera en sträng till gemener."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara användbart i många olika situationer. Det kan till exempel vara användbart för att jämföra strängar eller för att se till att de visas korrekt på skärmen.

## Hur man gör det

För att konvertera en sträng till små bokstäver i Haskell kan vi använda den inbyggda funktionen `map`. Detta gör vi genom att först definiera en funktion som konverterar en enskild bokstav till små bokstäver och sedan applicera denna funktion på varje bokstav i strängen med hjälp av `map`.

```Haskell
toLowerCase :: Char -> Char
toLowerCase c = if c `elem` ['A'..'Z']
                    then toEnum (fromEnum c + 32) :: Char
                    else c

map toLowerCase "HEJ" -- ger "hej" som output
```

Vi kan också använda följande kortare version med vanliga funktionskompositioner:

```Haskell
toLowerCase :: String -> String
toLowerCase = map toLower

toLowerCase "HELLO" -- ger "hello" som output
```

## Djupdykning

En viktig aspekt att tänka på när man konverterar strängar till små bokstäver i Haskell är hantering av specialtecken och tecken med diakritiska eller akuta markeringar. Dessa tecken kan vara olika beroende på vilken teckenuppsättning som används. En lösning på detta problem är att använda funktionen `toCaseFold` från modulen `Data.Text`.

```Haskell
import Data.Text (toCaseFold)

toCaseFold "ÅÄÖ" -- ger "åäö" som output oavsett vilken teckenuppsättning som används
```

Det är också viktigt att notera att funktionerna `map` och `toLower` endast fungerar på tecken och inte hela strängar. För att konvertera en hel sträng kan vi använda `map` tillsammans med `unpack` och `pack` från `Data.Text`.

## Se även

- [Haskell Wiki: Strings](https://wiki.haskell.org/Strings)
- [hackage: Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html) 
- [hackage: Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html)