---
title:                "Haskell: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy dany katalog istnieje, jest ważnym aspektem programowania w Haskellu. Po pierwsze, może to pomóc uniknąć błędów w programie związanych z brakującym katalogiem. Po drugie, może to być przydatne w przypadku działań na plikach, gdzie konieczne jest określenie ścieżki do istniejącego katalogu. W tym artykule omówimy, dlaczego warto zwrócić uwagę na sprawdzanie istnienia katalogów oraz jak to zrobić w Haskellu.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje, jest możliwe dzięki funkcji ```doesDirectoryExist``` z modułu ```System.Directory```. Wymaga to jednak wcześniejszego importowania tego modułu. Przykładowy kod może wyglądać następująco:

```Haskell
import System.Directory

main :: IO()
main = do
  let dirName = "moj_katalog"
  dirExists <- doesDirectoryExist dirName
  if dirExists
    then putStrLn "Katalog istnieje."
    else putStrLn "Katalog nie istnieje."
```

W powyższym przykładzie, najpierw importujemy potrzebny nam moduł, a następnie tworzymy zmienną z nazwą naszego katalogu. W kolejnej linii wywołujemy funkcję ```doesDirectoryExist```, która zwraca wartość typu ```Bool```, informującą czy dany katalog istnieje. W składni ```if/else``` sprawdzamy tę wartość i wypisujemy odpowiedni komunikat.

Przykładowy output programu może wyglądać tak:

```
Katalog istnieje.
```

## Deep Dive

Funkcja ```doesDirectoryExist``` korzysta z funkcji systemowej ```stat```, która jest dostępna na większości platform. Wynik tej funkcji jest konwertowany na ```Bool``` i zwracany przez Haskell. W przypadku błędnego wywołania lub gdy operacja nie powiedzie się, funkcja zwraca ```False```.

Warto również zwrócić uwagę, że funkcja ```doesDirectoryExist``` nie sprawdza, czy dany katalog jest dostępny do zapisu lub odczytu. Po prostu informuje nas o tym, czy dana ścieżka odpowiada prawdziwemu katalogowi.

## Zobacz także

- [Dokumentacja modułu System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Tutorial o zarządzaniu plikami i katalogami w Haskellu](https://www.fpcomplete.com/blog/2016/10/read-only-file-io#file-system-basics)
- [Poradnik programowania w Haskellu](https://wiki.haskell.org/How_to_write_a_Haskell_program)