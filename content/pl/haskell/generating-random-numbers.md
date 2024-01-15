---
title:                "Generowanie losowych liczb"
html_title:           "Haskell: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważnym aspektem wielu programów i algorytmów. W języku Haskell istnieją wielowymiarowe sposoby na generowanie losowych liczb, co czyni go dobrze przystosowanym do takiej potrzeby.

## Jak to zrobić

```Haskell
import System.Random
randomNumber :: IO Int
randomNumber = getStdRandom (randomR (1,10))
main = do
  number <- randomNumber
  putStrLn ("Twoja wylosowana liczba to: " ++ show number)
```

Ten przykład kodu wykorzystuje moduł System.Random, który dostarcza funkcje do generowania losowych liczb. W powyższym kodzie używamy funkcji getStdRandom, która zwraca losową liczbę typu Int w podanym zakresie (w tym przypadku od 1 do 10). Następnie wykorzystujemy funkcję putStrLn, aby wypisać wylosowaną liczbę wraz z tekstem dla użytkownika.

## Głębszy wgląd

Generowanie losowych liczb jest związane z koncepcją tzw. "przykładów niedeterministycznych". W języku Haskell można wykorzystać generator liczb pseudolosowych do stworzenia pewnego stanu, który możemy nadal manipulować w wyrażeniu wartościowym. Dzięki temu możemy tworzyć różne wartości przy każdym uruchomieniu programu.

## Zobacz także

- Dokumentacja modułu System.Random: https://hackage.haskell.org/package/random/docs/System-Random.html
- Przykłady generowania losowych liczb w języku Haskell: https://wiki.haskell.org/Random_Numbers