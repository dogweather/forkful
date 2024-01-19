---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer innebär att skapa nummer på ett sätt som inte kan predikteras bättre än av slumpen. Programmerare gör detta för att simulera oväntad input, generera unika identifierare, eller för att skapa slumpmässigt beteende i spel.

## Så här gör du:
Haskell erbjuder en enkel lösning för att generera slumpmässiga nummer genom `System.Random`-biblioteket. Använd funktionen `randomRIO` för att generera ett slumpmässigt nummer inom ett visst intervall. 

```Haskell
import System.Random

main = do 
   randomNum <- randomRIO (1::Int, 6) 
   putStrLn ("Generated random number: " ++ show randomNum)
```
När du kör detta program kan utmatningen vara något i stil med:
```
Generated random number: 4
```

## Djupdykning
Att generera slumpmässiga nummer är en gammal matematisk utmaning och ämnet för mycket forskning. Haskell's `System.Random`-biblioteket använder sig av en algoritm som kallas Mersenne Twister, efter två japanska matematiker som skapade den 1996.

Det finns dock alternativ till `System.Random`. `tf-random` och `mwc-random` är två bibliotek som erbjuder snabbare och mer kryptografiskt säkra slumpnumreringsfunktioner. 

Enda begränsningen att notera är att Haskell's random funktioner inte är helt "rena" - de kräver användning av `IO` för att utföra sitt arbete. 

## Se även
Websites med mer information och diskussion om generering av slumpmässiga nummer i Haskell:
- [Random Number Generation in Haskell: A Complicated Story](https://kowainik.github.io/posts/haskell-mini-patterns#random-number-generation)
- [Good Practices for Random in Haskell](https://alex-charlton.com/posts/Good_practices_for_Random_in_Haskell/)
- [Randomness and Haskell, Part 1](http://www.randomhacks.net/2007/02/21/randomness-and-haskell-part-1/)
- [Haskell | Random Numbers](https://www.geeksforgeeks.org/haskell-random-numbers/)