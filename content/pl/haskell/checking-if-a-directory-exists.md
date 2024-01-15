---
title:                "Sprawdzanie, czy istnieje katalog."
html_title:           "Haskell: Sprawdzanie, czy istnieje katalog."
simple_title:         "Sprawdzanie, czy istnieje katalog."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, czy dany katalog istnieje w Twoim systemie plików? Jest to ważne pytanie, gdyż może pomóc Ci uniknąć niepotrzebnych błędów w Twoim kodzie. W tym artykule poznamy sposób na sprawdzenie istnienia katalogu w języku Haskell.

## Jak to zrobić

Sprawdzenie istnienia katalogu w języku Haskell jest bardzo proste. Wystarczy użyć funkcji `doesDirectoryExist` z modułu `System.Directory`. Funkcja ta przyjmuje jako argument ścieżkę do katalogu, który chcemy sprawdzić. Poniżej znajduje się przykładowy kod:

```Haskell
import System.Directory

main = do
    exists <- doesDirectoryExist "sciezka/do/katalogu"
    putStrLn $ "Katalog istnieje: " ++ show exists
```

Wywołanie funkcji `doesDirectoryExist` zwraca wartość typu `Bool`, więc możemy ją wykorzystać w wyrażeniu `if` do podejmowania odpowiednich działań w zależności od wyniku. Poniżej znajduje się przykładowy kod, który wyświetla komunikat, jeśli katalog istnieje:

```Haskell
import System.Directory

main = do
    exists <- doesDirectoryExist "sciezka/do/katalogu"
    if exists
        then putStrLn "Katalog istnieje"
        else putStrLn "Katalog nie istnieje"
```

## Głębsza analiza

Sprawdzenie istnienia katalogu może być również przydatne w przypadku, gdy chcemy upewnić się, że nie usuniemy istniejącego katalogu przy wykonywaniu operacji na plikach. Możemy to osiągnąć wywołując funkcję `doesDirectoryExist` przed wykonaniem operacji.

Warto również wspomnieć, że funkcja `doesDirectoryExist` jest częścią modułu `System.Directory`, który oferuje wiele innych przydatnych funkcji do zarządzania plikami i katalogami w systemie.

## Zobacz także

- [Hackage: System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Stack Overflow: Check if Directory Exists Haskell](https://stackoverflow.com/questions/35522052/check-if-directory-exists-haskell)