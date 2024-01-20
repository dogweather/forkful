---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego polega na odczytaniu danych zapisanych w formie tekstu ze zwykłego pliku. Programiści robią to, aby manipulować danymi, przetwarzać je, a czasem po prostu wyświetlać użytkownikom.

## Jak to zrobić:

Wystarczy kilka linii kodu do odczytu pliku w Haskellu. Sprawdź nasze przykłady poniżej:

```Haskell
import System.IO  

main = do  
    handle <- openFile "plik.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle
```

Efekt końcowy powinien wyglądać tak:

```Haskell
Hello, Haskell!
```

## Głębsze spojrzenie:

- Kontekst historyczny: Haskell jest językiem programowania o wysokim poziomie abstrakcji, który istnieje od lat 90-tych. Jego zdolność do obsługi plików rozwinęła się w miarę ewolucji języka i potrzeb współczesnego programowania.
   
- Alternatywy: Można również użyc funkcji 'readFile' do odczytywania danych z pliku tekstowego:

```Haskell
main = do  
    contents <- readFile "plik.txt"  
    putStr contents
```

- Szczegóły implementacji: Kiedy otwierasz plik za pomocą funkcji 'openFile' w Haskellu, zdobywasz uchwyt na plik. Używasz tego uchwytu, aby później odczytywać zawartość pliku. Ważne jest, aby pamiętać o zamknięciu uchwytu na koniec.

## Zobacz też:

- Dokumentacja Haskell: https://www.haskell.org
- Obsługa plików w Haskellu: http://learnyouahaskell.com/input-and-output  
- 'System.IO' w Haskell: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html