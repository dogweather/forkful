---
title:    "Haskell: Sprawdzanie czy istnieje katalog"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu niezbędne jest sprawdzenie, czy dany katalog istnieje. Może to być konieczne, na przykład przy tworzeniu nowych plików lub manipulowaniu już istniejącymi. W tym wpisie dowiesz się, jak w prosty sposób sprawdzić istnienie katalogu w języku Haskell.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje, w Haskellu jest bardzo proste. Wystarczy skorzystać z funkcji `doesDirectoryExist` z modułu `System.Directory`. Poniżej znajduje się przykładowy kod, który sprawdzi istnienie katalogu o nazwie "test":

```Haskell
import System.Directory

main :: IO ()
main = do
    let directory = "test"
    exists <- doesDirectoryExist directory
    if exists
        then putStrLn "Katalog istnieje!"
        else putStrLn "Katalog nie istnieje!"
```

Po uruchomieniu tego kodu, w zależności od istnienia katalogu "test", otrzymamy odpowiednie wyjście:

```
Katalog istnieje!
```

lub

```
Katalog nie istnieje!
```

W powyższym przykładzie zmienną `exists` przypisujemy wartość logiczną `True` lub `False` w zależności od istnienia katalogu. Następnie w prosty sposób możemy wypisać odpowiedni komunikat.

## Deep Dive

Sprawdzanie istnienia katalogu opiera się na wywołaniu systemowej funkcji `stat`, która zwraca informacje o danym pliku lub katalogu. W Haskellu, aby wywołać tę funkcję, wykorzystujemy moduł `System.Posix.Files`.

Funkcja `doesDirectoryExist` jest w rzeczywistości pomocniczą funkcją, która wywołuje funkcję `getFileStatus` z modułu `System.Posix.Files`. Otrzymujemy w ten sposób obiekt `FileStatus`, który zawiera m.in. informację o typie pliku - czy jest to katalog, czy plik.

Możemy również wykorzystać funkcję `getFileStatus` bezpośrednio, aby otrzymać informacje o dowolnym pliku lub katalogu. Na przykład:

```Haskell
import System.Posix.Files

main :: IO ()
main = do
    fileStatus <- getFileStatus "test"
    let isDirectory = isDirectory fileStatus
    putStrLn $ "Czy plik 'test' jest katalogiem? " ++ show isDirectory
```

Po uruchomieniu tego kodu, otrzymamy następujące wyjście:

```
Czy plik 'test' jest katalogiem? True
```

Warto również wspomnieć, że funkcja `getFileStatus` działa tylko w systemach Unixowych, dlatego nie można jej wykorzystać w środowisku Windows.

## Zobacz również

- <https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html>
- <https://hackage.haskell.org/package/unix-2.7.2.2/docs/System-Posix-Files.html>
- <https://stackoverflow.com/questions/32614799/check-if-a-file-is-a-directory-haskell>