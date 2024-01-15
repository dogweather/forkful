---
title:                "Omvandla en sträng till gemener."
html_title:           "Haskell: Omvandla en sträng till gemener."
simple_title:         "Omvandla en sträng till gemener."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) är en användbar funktion när man behöver göra jämförelser av texter eller när man vill formatera text på ett enhetligt sätt.

## Så här gör du

Det enklaste sättet att konvertera en sträng till gemener i Haskell är genom att använda standardfunktionen `map` tillsammans med `toLower` från paketet `Data.Char`. Här är ett exempel där vi konverterar en sträng till gemener:

```Haskell
import Data.Char (toLower)

lowerCaseString :: String -> String
lowerCaseString str = map toLower str

main = do
  let str = "Hej, HASKELL är KUL!"
  putStrLn $ lowerCaseString str
  -- hej, haskell är kul!
```

Vi kan också använda en rekursiv funktion för att konvertera strängen stegvis. Det här är ett lite mer avancerat exempel, men det ger oss också möjlighet att klara av speciella tecken som åäö:

```Haskell
import Data.Char (toLower)

lowerChar :: Char -> Char
lowerChar c
  | c `elem` ['A'..'Z'] = toLower c
  | otherwise = c

lowerCaseString :: String -> String
lowerCaseString [] = []
lowerCaseString (c:str) = lowerChar c : lowerCaseString str

main = do
  let str = "Hej, HASKELL är KUL!"
  putStrLn $ lowerCaseString str
  -- hej, haskell är kul!
```

## Djupdykning

För att förstå hur dessa funktioner fungerar i bakgrunden behöver vi först titta på paradigmen "funktionell programmering". Till skillnad från imperativ programmering där man utför stegvisa instruktioner, använder sig funktionell programmering av funktioner som matar in värden och returnerar resultat. Genom att använda funktioner som `map` och `toLower` kan vi konvertera en hel sträng till gemener på ett enkelt sätt.

För att gå djupare in på "funktionell programmering" kan du kika på dessa länkar:

- [Funktionell programmering på svenska](https://sv.wikipedia.org/wiki/Funktionell_programmering)
- [Funktionella språk till skillnad från imperativa språk](https://www.bitbybitcoding.com/blog/5-functional-languages)
- [Varför välja funktionell programmering?](https://felixt.se/blog/2015/11/24/why-i-like-functional-programming/)

## Se även

Här är några användbara länkar för att utforska mer om att konvertera strängar till gemener i Haskell:

- [Dokumentation för standardfunktionen `map`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:map)
- [Dokumentation för funktionen `toLower`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#v:toLower)
- [Haskell Tutorial: Functions and Recursion](http://learnyouahaskell.com/higher-order-functions)
- [Allt om funktionell programmering](https://www.smashingmagazine.com/2014/07/dont-be-scared-of-functional-programming/)