---
title:                "Generering av slumpmässiga nummer"
html_title:           "Haskell: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generera slumpmässiga nummer är en vanlig uppgift bland programmerare. Det är helt enkelt processen att skapa en sekvens av nummer som verkar slumpmässigt. Detta kan vara användbart för att testa algoritmer, simuleringar eller spel, och det ger en annan dimension av variabilitet till en annars statisk kod.

## Så här:
``` Haskell
import System.Random

-- Genererar ett slumpmässigt heltal från 1 till 10
randomInt :: IO Int
randomInt = randomRIO (1, 10)

-- Genererar ett slumpmässigt flyttal från 0 till 1
randomFloat :: IO Float
randomFloat = randomIO

-- Genererar en sekvens av 5 slumpmässiga heltal från 1 till 100
randomList :: IO [Int]
randomList = replicateM 5 (randomRIO (1, 100))

-- Exempel på utskrift:
-- 9
-- 0.5977689417028006
-- [77, 49, 20, 13, 82]
```

## Djupdykning:
Att generera slumpmässiga nummer har varit en utmaning sedan de första elektroniska datorerna uppfanns. Historiskt sett har olika metoder använts, såsom att använda väderobservationer eller radioaktiv nedbrytning. Idag används vanligtvis pseudoslumpgeneratorer, vilket innebär att de genererar en sekvens av nummer som verkar slumpmässiga, men som i själva verket är deterministiska och baserade på en startpunkt kallad en seed. Det finns också andra metoder, som kryptografiska slumpgeneratorer, som ger högre kvalitet på slumpmässigheten. 

Alternativet till att generera slumpmässiga nummer är att använda statiska värden eller fördefinierade listor. Detta kan vara tillräckligt i vissa fall, men kan inte replikera den dynamiska och oförutsägbara naturen hos slumpmässiga nummer.

I Haskell implementeras slumpmässiga nummer med hjälp av en monad, vilket är en speciell typ av datatype som hanterar sidoeffekter. Detta ger en säker och enkel metod för att generera slumpmässiga nummer, samtidigt som det bibehåller programmets funktionella natur.

## Se även:
- [Random number generation in Haskell - Hackage](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Haskell Wiki - Random numbers](https://wiki.haskell.org/Random_numbers)