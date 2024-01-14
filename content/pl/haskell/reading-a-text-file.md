---
title:                "Haskell: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego

W dzisiejszych czasach programowanie jest niezwykle ważną umiejętnością, a jednym z najpopularniejszych języków programowania jest Haskell. W tym języku można napisać wydajny i elegancki kod, który jest również łatwy w utrzymaniu. Jednym z ciekawych zastosowań języka Haskell jest odczytywanie plików tekstowych. W tym artykule spróbujemy przybliżyć, dlaczego czytanie pliku tekstowego może być przydatnym umiejętnością dla każdego programisty.

# Jak to zrobić

Aby odczytać plik tekstowy w Haskell, możemy użyć funkcji `readFile` z modułu `System.IO`. Przyjmuję ona argument w postaci ścieżki do pliku i zwraca jego zawartość w postaci napisu.

```Haskell
import System.IO

main = do
  content <- readFile "plik.txt"
  putStrLn content
```

W powyższym przykładzie używamy również funkcji `putStrLn` do wyświetlenia zawartości pliku na ekranie.

# Deep Dive

Kiedy już nauczymy się odczytywać pliki tekstowe, możemy zacząć eksperymentować z różnymi metodami przetwarzania ich zawartości. Dzięki użyciu funkcji `lines` możemy podzielić odczytaną zawartość na pojedyncze linie, a następnie wykorzystać inne funkcje do ich dalszej obróbki. Przykładowo, możemy policzyć ilość linii w pliku przy użyciu funkcji `length`.

```Haskell
import System.IO

main = do
  content <- readFile "plik.txt"
  let linesNumber = length (lines content)
  putStrLn ("Liczba linii w pliku to: " ++ show linesNumber)
```

Dodatkowo, możemy użyć funkcji `words`, aby podzielić zawartość pliku na pojedyncze słowa, co może być przydatne przy przetwarzaniu tekstu.

# Zobacz również

- [Dokumentacja języka Haskell](https://www.haskell.org/documentation/)
- [Inne przydatne przykłady kodu w Haskellu](https://www.haskell.org/documentation#cookbook)
- [Przykładowe projekty w Haskellu](https://github.com/topics/haskell)