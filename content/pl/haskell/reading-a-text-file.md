---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Haskell: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Odczytywanie plików tekstowych to proces czytania zawartości pliku przechowywanego na dysku lub urządzeniu. Programiści często korzystają z tej techniki, aby przetworzyć dane z pliku do programu lub wyświetlić zawartość pliku na ekranie.

## Jak to zrobić:

Sprawanie trzeba, by pobrać odpowiedni moduł biblioteki standardowej w języku Haskell - `System.IO`. Następnie, aby odczytać plik tekstowy używamy funkcji `readFile` i przekazujemy jej ścieżkę do pliku jako argument. Przykładowy kod wyglądać będzie następująco:

```Haskell
import System.IO

main = do
    content <- readFile "path/to/file.txt"
    putStr content
```

Wynik będzie wypisany na ekranie w postaci zawartości pliku tekstowego.

## Deep Dive:

Historia odczytywania plików jest długa i sięga początków programowania. Od początku tworzenia komputerów, programiści musieli znaleźć sposoby na przetwarzanie danych przechowywanych na dysku. Alternatywnymi metodami odczytu plików w języku Haskell są funkcje `openFile` i `hGetContents`. Dzięki nim można specyfikować sposób odczytu pliku, na przykład czy plik powinien być otwarty w trybie binarnym czy tekstowym.

Wykorzystanie funkcji `readFile` jest prostym sposobem na odczytywanie plików, ale może być niewystarczające w bardziej złożonych scenariuszach. W takim przypadku warto zapoznać się z dokumentacją `System.IO` w celu znalezienia odpowiednich funkcji do własnego zastosowania.

## Zobacz także:

- Dokumentacja funkcji `readFile` w bibliotece standardowej języka Haskell (link)