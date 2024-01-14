---
title:                "Haskell: Generowanie losowych liczb"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezbędne w wielu programowaniu, szczególnie w przypadku symulacji, testowania i gier. W Haskellu możemy wykorzystać wbudowaną bibliotekę `System.Random`, aby uzyskać dostęp do wydajnego generatora liczb losowych.

## Jak to zrobić

Aby użyć biblioteki `System.Random`, musimy najpierw ją zaimportować, dodając `import System.Random` na początku naszego pliku Haskell. Następnie możemy skonstruować nasz generator losowy, używając funkcji `mkStdGen`, np. `let gen = mkStdGen 42`, gdzie 42 jest naszym ziarnem, które wyznacza początkowy stan generatora.

Aby wygenerować pojedynczą losową liczbę typu `Int`, możemy użyć funkcji `random`, np. `let (randomNum, newGen) = random gen :: (Int, StdGen)`, gdzie `randomNum` to wygenerowana liczba, a `newGen` to aktualizowany generator. Następnie możemy użyć aktualizowanego generatora, aby wygenerować kolejną liczbę.

Jeśli chcemy wygenerować losową liczbę z określonego zakresu, możemy użyć funkcji `randomR`, np. `let (randomNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)`, gdzie `1` i `10` są odpowiednio najmniejszą i największą możliwą wartością. Możemy również wygenerować liczby zmiennoprzecinkowe używając funkcji `randomRIO`, która zwraca wynik w kontekście `IO`.

Możemy również wygenerować losową wartość typu `Bool` przy użyciu funkcji `randomIO`, która zwraca `True` lub `False` w kontekście `IO`.

Przykładowy kod:

```
import System.Random

-- konstrukcja generatora losowego
let gen = mkStdGen 42

-- generowanie pojedynczej liczby
let (randomNum, newGen) = random gen :: (Int, StdGen)

-- generowanie liczby z określonego zakresu
let (randomNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)

-- generowanie liczby zmiennoprzecinkowej
randomDouble <- randomRIO (0.0, 1.0) :: IO Double

-- generowanie wartości typu Bool
randomBool <- randomIO :: IO Bool
```

## Głębsze zagadnienia

Biblioteka `System.Random` używa algorytmu generatora liczb pseudolosowych o nazwie "Mersenne Twister", który jest jednym z najbardziej popularnych i wydajnych algorytmów w tej dziedzinie. Algorytm ten jest oparty na liniowym sprężynowym generowaniu liczb pseudolosowych i zapewnia dobrą równomierność wygenerowanych liczb.

Warto również zwrócić uwagę na to, że generator losowy jest w stanie produkować tylko liczby "pseudo" losowe, co oznacza, że wyniki mogą być przewidywalne, jeśli znamy początkowy stan generatora. Dlatego też ważne jest, aby używać odpowiednio dużych ziaren, aby uzyskać jak największą losowość.

## Zobacz też

- Dokumentacja biblioteki `System.Random` w [Hoogle](https://hoogle.haskell.org/package/random-1.2.0/docs/System-Random.html)
- Strona internetowa Algorytmu "Mersenne Twister" [http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html)