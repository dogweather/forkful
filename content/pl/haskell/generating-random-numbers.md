---
title:                "Haskell: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego
Z pewnością wiele z was słyszało o generowaniu liczb losowych w programowaniu, ale dlaczego jest ono tak ważne? Jest to krytyczny element wielu algorytmów i programów, który pozwala na tworzenie różnych wariantów, a także symulowanie prawdziwego świata. Generowanie łatwo dostępnych liczb losowych w Haskellu jest nie tylko przydatne, ale także bardzo proste.

## Jak to zrobić
Do generowania liczb losowych w Haskellu możemy użyć wielu różnych funkcji z pakietu `System.Random`. Poniższy kod pokazuje przykładowe użycie funkcji `randomR` do wygenerowania losowej liczby całkowitej z podanego zakresu:

```Haskell
import System.Random

randomNumber :: IO Int
randomNumber = randomR (1, 10)

main :: IO ()
main = do
    number <- randomNumber
    print number
```

Powyższy kod wygeneruje losową liczbę całkowitą z zakresu od 1 do 10 i wypisze ją na ekranie. Możemy również użyć funkcji `random` do wygenerowania wartości typu `Double`. Poniższy kod pokazuje, jak wygenerować losową liczbę zmiennoprzecinkową z przedziału od 0 do 1:

```Haskell
import System.Random

randomNumber :: IO Double
randomNumber = random

main :: IO ()
main = do
    number <- randomNumber
    print number
```

Pamiętajmy, że każde wywołanie funkcji `random` lub `randomR` zwraca wartość typu IO, dlatego musimy użyć funkcji `do` i `<-` aby wydobyć wygenerowaną wartość.

## Wnikliwa obserwacja
Generowanie liczb losowych w programowaniu jest często wykorzystywane do losowego wybierania elementów z listy lub do symulacji zdarzeń losowych. W języku Haskell istnieje również możliwość generowania losowych wartości typów zdefiniowanych przez użytkownika, co może być bardzo przydatne w niektórych sytuacjach.

Możemy również ustawić ziarno (seed) dla generatora liczb losowych, aby uzyskać deterministyczne wyniki. W tym celu możemy użyć funkcji `mkStdGen` i przekazać jej dowolną liczbę całkowitą. Poniższy kod pokazuje, jak generować losowe wartości z ustalonym ziarnem:

```Haskell
import System.Random

main :: IO ()
main = do
    let seed = mkStdGen 42   -- ustawiamy ziarno na 42
    let randomNumber = randomR (1, 100) seed :: (Int, StdGen) -- wygeneruj liczbę całkowitą z zakresu od 1 do 100 wraz z informacją o nowym ziarnie
    print randomNumber     -- wyświetlamy wygenerowaną liczbę i nowe ziarno
```

Jedną z zalet ustawiania ziarna jest możliwość powtarzalności wygenerowanych wyników, co może być przydatne w celach testowania aplikacji.

## Zobacz także
* [Dokumentacja pakietu `System.Random`](https://hackage.haskell.org/package/random/docs/System-Random.html)
* [Tutorial o generowaniu liczb losowych w Haskellu](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-random-numbers)
* [Przykłady użycia w okolicy rand](https://stackoverflow.com/questions/24894498/haskell-10-million-generations-and-checking-all-numbers-for-a-pattern)