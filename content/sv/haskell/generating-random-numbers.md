---
title:                "Haskell: Generera slumpmässiga tal"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att använda slumpmässiga nummer i programmering kan vara användbart för att skapa mångsidiga och intressanta algoritmer, simuleringar och spel.

## Hur man gör det

Att generera slumpmässiga nummer i Haskell är relativt enkelt. Det första steget är att inkludera "System.Random" modulen i ditt program. Sedan kan du använda funktionen "getStdRandom" för att få ett slumpmässigt nummer inom ett givet intervall.

```Haskell
import System.Random

randomNum :: IO Int
randomNum = getStdRandom (randomR (1,10)) -- genererar ett slumpmässigt heltal mellan 1 och 10

main = do
    num <- randomNum
    print num -- utskrift: ett slumpmässigt nummer mellan 1 och 10
```

Om du vill generera ett slumpmässigt flyttal istället för ett heltal, kan du använda "randomRIO" funktionen istället för "getStdRandom".

```Haskell
import System.Random

randomFloat :: IO Float
randomFloat = randomRIO (1.0, 100.0) -- genererar ett slumpmässigt flyttal mellan 1.0 och 100.0

main = do
    num <- randomFloat
    print num -- utskrift: ett slumpmässigt flyttal mellan 1.0 och 100.0
```

## Djupdykning

Att generera slumpmässiga nummer i Haskell använder sig av en generering av sanna slumpmässiga nummer baserade på en seed (frö). En seed är ett startvärde för algoritmen som används för att generera slumpmässiga nummer. På så sätt kan funktionerna "getStdRandom" och "randomRIO" garantera att de genererar sanna slumpmässiga nummer.

Om du vill använda samma seed för att alltid generera samma serie av slumpmässiga nummer, kan du använda "setStdGen" funktionen tillsammans med "mkStdGen" för att skapa en seed baserad på ett specifikt heltal.

```Haskell
import System.Random

randomList :: IO [Int]
randomList = do
    gen <- getStdGen
    let (num, newGen) = randomR (0,10) gen
    setStdGen (mkStdGen 42) -- seeden 42 ger alltid samma serie av nummer
    return (take 10 (randomRs (1,10) newGen))

main = do
    numList <- randomList
    print numList -- utskrift: [7,2,6,10,7,2,3,6,8,2]
```

## Se även

- [Haskell Random Library](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Slumpmässiga tal i Haskell](https://www.geeksforgeeks.org/random-numbers-in-haskell/)
- [Haskell-Communityn på Stack Overflow](https://stackoverflow.com/questions/tagged/haskell)