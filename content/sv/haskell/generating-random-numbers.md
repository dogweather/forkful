---
title:                "Haskell: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Många programmerare väljer att använda slumpmässigt genererade nummer för att skapa variation och osäkerhet i sina program. Det är en vanlig teknik inom spelutveckling, simuleringar och kryptering.

## Så här gör du

För att generera slumpmässiga nummer i Haskell, behöver du importera modulen "Random". Sedan kan du använda funktioner som "randomR" för att generera ett slumpmässigt nummer inom ett visst intervall eller "randomIO" för ett slumpmässigt nummer utanför ett intervall.

```Haskell
import System.Random

-- Genererar ett slumpmässigt heltal mellan 1 och 10
randomR (1,10) :: IO Int

-- Genererar ett slumpmässigt flyttal mellan 0 och 1
randomIO :: IO Float
```

Det är också möjligt att generera en lista av slumpmässiga nummer med hjälp av funktionen "randoms". Denna funktion tar också en slumpmässig generator som input och genererar en oändlig lista av slumpmässiga nummer.

```Haskell
import System.Random

-- Genererar en oändlig lista av slumpmässiga heltal mellan 1 och 10
randoms (mkStdGen 42) :: [Int]
```

## Djupdykning

Bakom kulisserna använder Haskell en teknik som kallas "pseudo-random number generation". Det innebär att den slumpmässiga generatorn faktiskt inte är helt slumpmässig, utan baseras på en startblandning av nummer som sedan utvecklas med hjälp av en matematisk algoritm.

Det finns också möjlighet att använda en "slumpmässig seed" för att kontrollera vilken blandning av nummer som genererar genom att ange en startpunkt för den slumpmässiga generatorn.

Slumpmässiga nummer som genereras i Haskell är också "pure", vilket betyder att de inte påverkas av omgivningen eller andra faktorer. Detta gör dem lämpliga för tester och för att upprepa samma händelser i ett program.

## Se även

- [Haskell Random module documentation](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [A Gentle Introduction to Haskell - Randomness](https://www.haskell.org/tutorial/randomness.html)
- [Learn You a Haskell for Great Good! - Randomness](http://learnyouahaskell.com/input-and-output#randomness)