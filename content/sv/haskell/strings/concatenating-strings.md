---
date: 2024-01-20 17:35:01.698118-07:00
description: "S\xE5 g\xF6r du: I Haskell kan du konkatenera str\xE4ngar med flera\
  \ tekniker. H\xE4r \xE4r n\xE5gra exempel."
lastmod: '2024-03-13T22:44:37.947338-06:00'
model: gpt-4-1106-preview
summary: "I Haskell kan du konkatenera str\xE4ngar med flera tekniker."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## Så gör du:
I Haskell kan du konkatenera strängar med flera tekniker. Här är några exempel:

```Haskell
-- Använda (++) operatorn
main :: IO ()
main = do
    let hello = "Hej, "
    let world = "världen!"
    putStrLn (hello ++ world)
```
Output: `Hej, världen!`

Eller för att sammanfoga en lista med strängar:

```Haskell
main :: IO ()
main = do
    let words = ["Haskell", " ", "är", " ", "kul!"]
    putStrLn (concat words)
```
Output: `Haskell är kul!`

Och för mycket stora strängar eller när prestanda är viktig, använd `Data.Text`:

```Haskell
import qualified Data.Text as T

main :: IO ()
main = do
    let hello = T.pack "Hej, "
    let world = T.pack "världen!"
    T.putStrLn (T.append hello world)
```
Output: `Hej, världen!`

## Djupdykning
Konkatenering härstammar från latinets "concatenare", vilket betyder att länka ihop. I tidiga programmeringsspråk var stringhantering en komplex process. Haskell, infört på 1990-talet, förenklade detta med sin rena syntaktiska stil.

Alternativ till konkatenering inkluderar `intercalate`, som sätter ihop element i en lista med ett givet skiljetecken, och `Data.Text` biblioteket som erbjuder en mer effektiv hantering för stora textmassor genom minnesbesparande strukturer och "lazy" utvärdering.

I Haskell utförs konkatenering med hjälp av den lata utvärderingsmodellen, vilket betyder att beräkningar sker endast när det är nödvändigt, vilket kan vara mer effektivt än i strikta språk.

## Se även:
- [Haskell `Data.Text` dokumentation](https://hackage.haskell.org/package/text)
- [Haskell Wiki om strängar](https://wiki.haskell.org/Strings)
- [Learn You a Haskell for Great Good! av Miran Lipovača](http://learnyouahaskell.com/starting-out#an-intro-to-lists)
