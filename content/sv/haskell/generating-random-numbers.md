---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:22.977748-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal innebär att skapa tal som inte är förutsägbara av människor. Programmerare använder det för att simulera slumpmässiga händelser, testa algoritmer och för spellogik.

## Hur gör man:
```haskell
import System.Random (randomRIO)

-- Generera ett slumpmässigt heltal mellan 1 och 100
randomNumber :: IO Int
randomNumber = randomRIO (1, 100)

main :: IO ()
main = do
    num <- randomNumber
    print num
```
Kör `main` för att se ett slumpmässigt nummer, t.ex. `42`.

## Djupdykning
Funktionen `randomRIO` i Haskell används för IO-bunden slumptalsgenerering. Haskell lanserades 1990 och har ända sedan dess haft starkt stöd för matematiska operationer, inklusive slumptalsgenerering. Jämfört med att direkt använda `rand` i språk som C, så hanterar Haskell slumptalsfrön bättre och säkrare genom sitt typsystem och rena funktioner. Funktionen `randomRIO` gömmer komplexiteten med frön och tillstånd i IO-monaden. Alternativ som biblioteket `random-fu` finns för de som söker mer kontroll och funktionalitet.

## Se även:
- [Hackage - System.Random](https://hackage.haskell.org/package/random)
- [LYAH - For a Few Monads More](http://learnyouahaskell.com/for-a-few-monads-more#io-monad)
