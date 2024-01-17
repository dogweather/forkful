---
title:                "Sprawdzanie istnienia folderu"
html_title:           "Haskell: Sprawdzanie istnienia folderu"
simple_title:         "Sprawdzanie istnienia folderu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Sprawdzanie czy istnieje folder to proces, w którym sprawdzamy czy dana ścieżka na naszym komputerze odpowiada za istnienie folderu. Programiści wykonują tę czynność, aby upewnić się, że ich program działa poprawnie i aby uniknąć błędów związanych z brakiem folderu.

## Jak to zrobić:
```Haskell
import System.Directory  -- moduł do obsługi operacji na plikach i folderach

main :: IO ()
main = do
  let path = "ścieżka/do/folderu"  -- zmienna z ścieżką folderu
  dirExists <- doesDirectoryExist path  -- funkcja sprawdzająca czy folder istnieje
  if dirExists
    then putStrLn "Folder istnieje!"
    else putStrLn "Brak folderu... :("
```

## Głębsze wody:
1. Kontekst historyczny:
Sprawdzanie czy istnieje folder było często używaną czynnością w starszych językach programowania, takich jak C i C++, ponieważ nie zawierały one wbudowanych funkcji do obsługi operacji na plikach i folderach.

2. Alternatywy:
Istnieją inne sposoby na sprawdzenie czy istnieje folder, takie jak użycie biblioteki "System.Directory.Tree". Jednak używanie wbudowanych funkcji jest łatwiejsze i szybsze.

3. Szczegóły implementacji:
Funkcja "doesDirectoryExist" wykorzystuje metodę "stat" systemu operacyjnego, która zwraca informacje o danym pliku lub folderze. Jeśli folder istnieje, metoda zwróci informacje o nim, w przeciwnym razie zwróci błąd.

## Zobacz też:
- [Dokumentacja System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Tutorial o obsłudze folderów w Haskellu](https://www.tutorialspoint.com/haskell/haskell_working_with_files.htm)