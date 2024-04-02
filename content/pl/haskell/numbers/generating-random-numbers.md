---
date: 2024-01-27 20:34:09.917010-07:00
description: "Generowanie liczb losowych w Haskellu polega na tworzeniu liczb, kt\xF3\
  re s\u0105 nieprzewidywalne wed\u0142ug ludzkich standard\xF3w. Jest to kluczowe\
  \ w scenariuszach\u2026"
lastmod: '2024-03-13T22:44:35.447334-06:00'
model: gpt-4-0125-preview
summary: "Generowanie liczb losowych w Haskellu polega na tworzeniu liczb, kt\xF3\
  re s\u0105 nieprzewidywalne wed\u0142ug ludzkich standard\xF3w. Jest to kluczowe\
  \ w scenariuszach\u2026"
title: Generowanie liczb losowych
weight: 12
---

## Co i dlaczego?

Generowanie liczb losowych w Haskellu polega na tworzeniu liczb, które są nieprzewidywalne według ludzkich standardów. Jest to kluczowe w scenariuszach sięgających od aplikacji kryptograficznych po symulacje, gdzie element przypadku jest wymagany do dokładnego modelowania zjawisk świata rzeczywistego.

## Jak to zrobić:

Aby generować liczby losowe w Haskellu, zwykle używa się pakietu `random`, który jest częścią platformy Haskell. Oto przewodnik krok po kroku:

Na początku upewnij się, że masz zainstalowany pakiet `random`. Jeśli nie, możesz go zdobyć przez Cabal lub Stack.

### Generowanie losowej liczby

Aby wygenerować prostą losową liczbę, możesz użyć funkcji `randomRIO`, która produkuje losową wartość w określonym zakresie.

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Random number: " ++ show randomNumber
```

### Generowanie listy liczb losowych

Generowanie listy liczb losowych jest nieco bardziej zaawansowane, ale nadal proste:

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  numbers <- randomList 5
  print numbers
```

Ten fragment kodu tworzy funkcję `randomList`, która generuje listę losowych liczb całkowitych. Zamień `(1, 100)` na żądany zakres.

## Wnikliwe spojrzenie

Pakiet `random` w Haskellu zapewnia generator liczb pseudolosowych (PRNG), co oznacza, że wygenerowane liczby nie są prawdziwie losowe, ale mogą wydawać się losowe dla wielu zastosowań. Rdzeń możliwości generowania liczb losowych w Haskellu leży w klasie typów `RandomGen`, która abstrahuje różne metody generowania liczb losowych, oraz w klasie typów `Random`, która zawiera typy, które mogą być generowane losowo.

Historycznie podejście Haskella do generowania liczb losowych podkreślało czystość i reprodukowalność. Dlatego operacje związane z losowością są wyraźnie obsługiwane w monadzie `IO` lub wymagają ręcznego przekazywania i aktualizowania stanów generatora — aby zachować przezroczystość odniesienia.

W niektórych zastosowaniach, takich jak kryptografia, pseudolosowe liczby generowane przez domyślny PRNG mogą nie być wystarczająco bezpieczne. W takich przypadkach programiści Haskell często zwracają się ku bardziej wyspecjalizowanym bibliotekom, takim jak `crypto-random`, które są zaprojektowane, aby spełniać rygorystyczne wymagania aplikacji kryptograficznych.

Co więcej, alternatywne biblioteki takie jak `mwc-random` oferują lepszą wydajność i jakość liczb losowych dla symulacji i innych zastosowań, implementując nowoczesne algorytmy takie jak Mersenne Twister.

Wybierając podejście do generowania liczb losowych w Haskellu, istotne jest, aby wziąć pod uwagę potrzeby aplikacji związane z jakością losowości, wydajnością i bezpieczeństwem, aby wybrać najbardziej odpowiednie narzędzie lub bibliotekę.
