---
title:                "Generering av tilfeldige tall"
html_title:           "Haskell: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har ønsket å legge til en tilfeldighetsfaktor i programmene dine, eller ønsker å lage et spill eller en simulering, så kan det å generere tilfeldige tall være nyttig.

## Hvordan
Å generere tilfeldige tall i Haskell er enkelt og intuitivt. Du kan bruke "random" funksjonen fra "System.Random" biblioteket. Her er et eksempel på hvordan du kan generere et tilfeldig tall mellom 1 og 10 og skrive det ut til konsollen:

```Haskell
import System.Random

main = do
  gen <- getStdGen
  let (num, _) = randomR (1, 10) gen :: (Int, StdGen)
  print num
```

Dette koden bruker standardgeneratoren "getStdGen" for å generere et tilfeldig tall, og "randomR" funksjonen for å lage et tall mellom 1 og 10. Det tilfeldige tallet er deretter lagret i variabelen "num" og skrevet ut ved hjelp av "print" funksjonen.

## Dypdykk
Haskell bruker en konsept som kalles for "lazy evaluation" som gjør det mulig å skape uendelige lister av tilfeldige tall. Her er et eksempel på en uendelig liste av tilfeldige tall mellom 1 og 100:

```Haskell
import System.Random

randomList :: Int -> [Int]
randomList seed = let gen = mkStdGen seed
                      num = randomRs (1, 100) gen
                    in num : randomList seed

main = do
  let numbers = randomList 4
  print (take 10 numbers)
```

I denne koden blir en uendelig liste av tilfeldige tall generert ved hjelp av "mkStdGen" funksjonen og "randomRs" som lager en liste av tilfeldige tall mellom 1 og 100. Denne listen blir deretter begrenset til 10 tall ved hjelp av "take" funksjonen, og skrevet ut til konsollen.

## Se også
- [Random Package Documentation](https://hackage.haskell.org/package/random)
- [Learn You a Haskell: For Great Good!](http://learnyouahaskell.com/)