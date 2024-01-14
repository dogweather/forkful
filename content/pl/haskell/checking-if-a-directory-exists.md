---
title:                "Haskell: Sprawdzenie czy katalog istnieje."
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy dany katalog istnieje, jest ważnym krokiem w wielu programach pisanych w języku Haskell. Może to pomóc w uniknięciu błędów lub umożliwić dostęp do potrzebnych informacji. Dlatego warto poznać jak to zrobić.

## Jak to zrobić

Aby sprawdzić istnienie katalogu w Haskell, można użyć funkcji `doesDirectoryExist` z modułu `System.Directory`. Przyjmuje ona ścieżkę do katalogu jako argument i zwraca wartość typu `Bool` (True/False).

Przykładowy kod:

```Haskell
import System.Directory

main = do
  let path = "C:\\Users\\User\\Documents\\example"
  dirExists <- doesDirectoryExist path
  if dirExists
    then putStrLn "Katalog istnieje."
    else putStrLn "Katalog nie istnieje."
```

Przykładowy wynik dla istniejącego katalogu:

```
Katalog istnieje.
```

Przykładowy wynik dla nieistniejącego katalogu:

```
Katalog nie istnieje.
```

## Głębsza analiza

W języku Haskell, funkcja `doesDirectoryExist` używa systemowego wywołania `access`, które sprawdza istnienie pliku lub katalogu. Oznacza to, że nie tylko można sprawdzić istnienie katalogu, ale także dowiedzieć się czy można odczytać, zapisać lub wykonać operacje na danym katalogu.

## Zobacz również

- Dokumentacja funkcji `doesDirectoryExist`: https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist
- Przykładowy kod z wykorzystaniem funkcji `doesDirectoryExist`: https://stackoverflow.com/questions/35661188/how-to-check-if-a-directory-exists-in-haskell