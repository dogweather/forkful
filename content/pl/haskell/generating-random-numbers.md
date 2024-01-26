---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:36.027840-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Generowanie liczb losowych to podstawa wielu aplikacji – od gier po symulacje. Programiści wykorzystują je dla różnorodności, nieprzewidywalności i testowania.

## How to: (Jak to zrobić:)
Rozpocznijmy od włączenia potrzebnego modułu i stworzenia prostego generatora.

```Haskell
import System.Random (newStdGen, randomRs)

-- Generowanie losowej listy liczb całkowitych
randomInts :: Int -> IO [Int]
randomInts seed = do
  gen <- newStdGen
  let numbers = take 10 $ randomRs (1, 100) gen
  return numbers

main = do
  randomNumbers <- randomInts 42
  print randomNumbers
```

Możesz oczekiwać wyniku typu:
```
[28, 36, 95, 78, 58, 62, 20, 82, 16, 95]
```

## Deep Dive (Dogłębna analiza)
Generowanie liczb losowych w Haskellu jest ciekawe. Haskell to język funkcyjny i ceni czyste funkcje, które zawsze zwracają tę samą wartość dla danych wejściowych. A liczby losowe? To przeciwieństwo. Rozwiązaniem jest użycie monady IO, by obsłużyć stan generowania.

Wcześniejsze wersje Haskell'a stosowały `System.Random`, ale współcześnie mamy lepsze opcje jak `random-fu` czy `mwc-random`, które oferują większą szybkość i lepszą dystrybucję.

Samo generowanie opiera się na algorytmach takich jak liniowy generator kongruencyjny (LCG) czy Mersenne Twister. Są to metody obliczania liczb, które wydają się być losowe, ale w rzeczywistości są całkowicie deterministyczne.

## See Also (Zobacz również)
- [Hackage - random package](https://hackage.haskell.org/package/random)
- [School of Haskell - Generating Random Data](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)
