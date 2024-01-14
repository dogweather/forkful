---
title:    "Haskell: Sprawdzanie czy istnieje katalog."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Czemu

Sprawdzenie, czy katalog istnieje, jest ważnym aspektem programowania w języku Haskell. Pozwala nam to upewnić się, że nasz kod będzie działał poprawnie i nie będzie wywoływał błędów związanych z brakiem katalogu.

## Jak to zrobić

Aby sprawdzić, czy dany katalog istnieje, musimy użyć funkcji *doesDirectoryExist*, która jest dostępna w bibliotece standardowej *System.Directory*. Funkcja ta przyjmuje jako argument ścieżkę do katalogu i zwraca wartość typu *IO Bool*, która określa, czy dany katalog istnieje.

Przykładowy kod wykorzystujący funkcję *doesDirectoryExist* może wyglądać następująco:

```Haskell
import System.Directory

main = do
    putStrLn "Podaj ścieżkę do katalogu:"
    path <- getLine
    directoryExists <- doesDirectoryExist path
    if directoryExists
        then putStrLn "Katalog istnieje."
        else putStrLn "Katalog nie istnieje."
```

Powyższy kod najpierw prosi użytkownika o podanie ścieżki do sprawdzenia, a następnie wykorzystuje funkcję *doesDirectoryExist*, aby ustalić, czy podany katalog istnieje. Na podstawie zwróconej wartości *IO Bool*, program wyświetli odpowiedni komunikat informujący o stanie katalogu.

## Głębszy wgląd

Sprawdzanie, czy katalog istnieje, może być ważne w różnych sytuacjach. Może nam to pomóc uniknąć błędów związanych z nieistniejącym katalogiem lub wykonać odpowiednie działania w zależności od tego, czy katalog istnieje czy nie. Funkcja *doesDirectoryExist* również wykorzystuje wywołanie systemowe, aby przetestować istnienie katalogu, co może być przydatne przy działaniu na różnych systemach operacyjnych.

## Zobacz także

* [Dokumentacja funkcji *doesDirectoryExist*](https://hackage.haskell.org/package/directory/docs/System-Directory-Internals.html#v:doesDirectoryExist)
* [Inne funkcje dostępne w bibliotece *System.Directory*](https://hackage.haskell.org/package/directory/docs/System-Directory.html)