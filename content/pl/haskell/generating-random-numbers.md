---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Generowanie liczb losowych to sposób nakreślenia przypadkowego ciągu liczb, które nie mają żadnego widocznego schematu. Programiści używają tych liczb m.in. do symulacji, gier czy bezpieczeństwa danych.

## Jak to zrobić:

Zacznijmy przez importowanie modułu `System.Random`. Wygenerujmy losową liczbę całkowitą:

```Haskell
import System.Random

main = do
  gen <- newStdGen
  print (take 5 $ randoms gen :: [Int])
```
W wyniku ujrzymy pięć liczb losowych.

## Głębsze spojrzenie

Generowanie liczb losowych jest z nami od czasów wcześniejszych komputerów. Ewoluowało wraz z rozwojem teorii prawdopodobieństwa i matematyki.

Ich Ważne jest, że generowane liczby są pseudolosowe; w rzeczywistości powstają z precyzyjnie określonego algorytmu. 

W Haskellu domyślnie używamy generatora Mersenne Twister, choć dostępne są inne (jak porównywalny Xorshift).

## Zobacz także:

Jak już potraficie tworzyć proste liczby losowe, możecie zainteresować się bardziej skomplikowanymi technikami. Tutaj są linki, które mogą pomóc:

2. [Dokumentacja System.Random](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
3. [Artykuł na temat jakości różnych generatorów liczb losowych](https://www.jstatsoft.org/article/view/v008i14)