---
title:    "Haskell: Sammanslagning av strängar"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Varför

Sammanfogning av strängar är en vanlig operation i många programmeringsspråk, inklusive Haskell. Det gör det möjligt för oss att skapa en större sträng genom att kombinera flera mindre delar. Det kan vara användbart när vi vill skapa dynamiska strängar baserat på variabler eller olika delar av en sträng.

# Hur man gör det

För att sammanfoga strängar i Haskell kan vi använda funktionen `++`. Den tar två strängar som argument och returnerar en ny sträng som innehåller båda strängarna sammanfogade.

```Haskell 
sträng1 ++ sträng2 
```

Se till att ha ett mellanslag mellan `++` och strängarna för att skapa en tydlig separation mellan dem i den nya strängen.

Vi kan också använda funktionen `concat` för att sammanfoga flera strängar samtidigt. I följande exempel, sätter vi strängarna i en lista och använder `concat` för att slå samman dem till en enda sträng.

```Haskell 
concat ["Hej", "på", "dig"] 

-- Output: Hej på dig
```

# Djupdykning

I Haskell är strängar faktiskt listor av tecken, vilket gör att vi kan använda många listfunktioner för att manipulera och sammanfoga dem. Till exempel kan vi använda `map` funktionen för att applicera en funktion på varje tecken i en sträng. I följande exempel, konverterar vi varje tecken till versaler med hjälp av `toUpper` funktionen från `Data.Char` modulen.

```Haskell 
import Data.Char (toUpper)

map toUpper "hej på dig!" 

-- Output: HEJ PÅ DIG!
```

Vi kan också använda listkomprehension för att sammanfoga flera strängar baserat på villkor. I följande exempel, filtrerar vi bort alla mellanslag från två strängar innan vi sammanslår dem.

```Haskell 
[str1 ++ str2 | c1 <- str1, c2 <- str2, c1 /= ' ', c2 /= ' '] 

-- Exempel: ["hejdå", "påsurban"]
```

# Se även

- [Haskell String Functions](https://www.tutorialspoint.com/haskell/haskell_string_functions.htm)
- [A Gentle Introduction to Haskell](https://www.haskell.org/tutorial/strings.html)
- [Haskell Wiki - String](https://wiki.haskell.org/String)