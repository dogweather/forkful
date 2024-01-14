---
title:    "Haskell: Generera slumpmässiga nummer"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Generering av slumpmässiga tal är en viktig del av många programmeringsuppgifter, från att skapa spel till att utforska komplexa algoritmer. Det är en kraftfull funktion som kan användas i många olika sammanhang och som kan tillföra variation och överraskning i dina program.

## Hur man gör det

I Haskell finns det flera olika sätt att generera slumpmässiga tal. Ett enkelt sätt är att använda funktionen `random` från modulen `System.Random`.

```Haskell
import System.Random

-- Genererar en slumpmässig Integer mellan 1-100
randomNum :: IO Integer
randomNum = getStdRandom (randomR (1,100))

-- Genererar en slumpmässig Float mellan 0-1
randomFloat :: IO Float
randomFloat = getStdRandom random 

-- Genererar en slumpmässig Char från a-z
randomChar :: IO Char
randomChar = getStdRandom (randomR ('a','z'))

main :: IO ()
main = do
  num <- randomNum
  putStrLn $ "Slumpmässig Integer: " ++ show num
  
  float <- randomFloat
  putStrLn $ "Slumpmässig Float: " ++ show float
  
  char <- randomChar
  putStrLn $ "Slumpmässig Char: " ++ show char
```

Output:
```
Slumpmässig Integer: 64
Slumpmässig Float: 0.25286472
Slumpmässig Char: 'g'
```

## Djupdykning

Internslingen bakom generering av slumpmässiga tal är en process som kallas "pseudo-random number generation". Det innebär att trots att talen framstår som slumpmässiga, så följer de en viss algoritm som genererar dem. För att få ett verkligt slumpmässigt tal behöver man använda externa faktorer som användarens input eller klockslaget på datorn. Detta kan göras genom att använda funktionen `getRandom` och ange en seed, som är en startpunkt för algoritmen.

```Haskell
import System.Random

randomSeed :: Int
randomSeed = 42

main :: IO ()
main = do
  num1 <- getRandom randomSeed
  num2 <- getRandom randomSeed
  putStrLn $ "Slumpmässig Integer 1: " ++ show num1
  putStrLn $ "Slumpmässig Integer 2: " ++ show num2
```

Output:
```
Slumpmässig Integer 1: 89
Slumpmässig Integer 2: 89
```

Att välja en seed och använda den kan vara användbart om man behöver återskapa samma serie av slumpmässiga tal. Det kan också användas för debugging och testning av programmet.

## Se även

- [System.Random dokumentation](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Haskell Cheat Sheet](https://github.com/wolfgangwalther/haskell-cheat-sheet/blob/master/HaskellCheatSheet.pdf)
- [Allt om Haskell](https://wais.badharvest.net/2011/02/19/allt-om-haskell-2/) (på svenska)