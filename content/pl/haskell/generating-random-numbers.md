---
title:    "Haskell: Generowanie liczb losowych"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią wielu programów w języku Haskell. To ważne narzędzie, które pozwala na tworzenie różnorodnych i złożonych aplikacji, takich jak gry, symulacje czy algorytmy optymalizacyjne.

## Jak to zrobić

Aby wygenerować losową liczbę w Haskellu, używamy funkcji `random` z modułu `System.Random`. Przykładowy kod wygląda następująco:

```Haskell
import System.Random

main = do
  x <- randomRIO (1, 10)   -- wygeneruje liczbę całkowitą w przedziale od 1 do 10
  print x                   -- wyświetli wylosowaną liczbę
```

Wywołanie funkcji `randomRIO` zwraca wartość typu `IO a`, dlatego musimy skorzystać z komendy `do` oraz operatora `<-`, aby odzyskać wylosowaną liczbę. Następnie możemy wyświetlić ją na ekranie przy użyciu funkcji `print`.

Innym sposobem na wygenerowanie losowej liczby jest użycie funkcji `random`, która zwraca wartość typu `StdGen -> (a, StdGen)`. Przykładowy kod wykorzystujący tę funkcję wygląda tak:

```Haskell
import System.Random

main = do
  gen <- newStdGen            -- tworzy nowy generator liczb losowych
  let (x, newGen) = random gen :: (Int, StdGen)  -- wygeneruje losową liczbę typu Int
  print x                     -- wyświetli wylosowaną liczbę
```

W obydwu przypadkach wartością możemy zwrócić inne typy danych, zmieniając je odpowiednio w definicjach funkcji. Więcej informacji na ten temat znajdziesz w sekcji "Głębsze zanurzenie".

## Głębsze zanurzenie

Generowanie losowych liczb w Haskellu odbywa się za pomocą generatora liczb losowych, który jest przechowywany w pamięci programu. Stan tego generatora jest modyfikowany przy użyciu funkcji `random`.

Jeśli chcemy kontrolować stan generatora, możemy użyć funkcji `newStdGen`, która tworzy nowy generator i zwraca go wraz z nowym stanem. Ponadto, możemy wykorzystać funkcję `mkStdGen`, aby utworzyć generator o określonym stanie.

W języku Haskell istnieje cały zbiór funkcji do generowania różnego rodzaju wartości losowych, takich jak liczby całkowite, liczby zmiennoprzecinkowe, wartości logiczne czy listy. Pełną listę funkcji znajdziesz w dokumentacji modułu `System.Random` oraz w linkach poniżej.

## Zobacz także

- Dokumentacja modułu `System.Random`: https://hackage.haskell.org/package/random/docs/System-Random.html
- Przykładowe zadania z użyciem generatorów liczb losowych w Haskellu: https://wiki.haskell.org/Random,_State_and_Continuations