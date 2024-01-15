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

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego niektóre programy potrafią odczytywać pliki tekstowe, podczas gdy inne nie? W tym artykule dowiecie się, jak w języku Haskell odczytać zawartość pliku tekstowego i wykorzystać ją w swoich programach.

## Jak to zrobić

Rozpocznijmy od importu modułu `System.IO`, który umożliwi nam wykorzystanie funkcji do obsługi plików. Następnie stwórzmy funkcję `readFile` i podajmy jako argument ścieżkę do pliku, który chcemy odczytać. Przykładowo:

```Haskell
import System.IO

main = do
    contents <- readFile "plik.txt"
    putStrLn contents
```

Wywołanie `readFile` zwróci nam zawartość pliku jako pojedynczy napis. Następnie wypisujemy ją na ekran za pomocą funkcji `putStrLn`. Koniec! Nasz program potrafi już czytać pliki tekstowe.

## Głębsze zanurzenie

Oczywiście, możliwości odczytywania plików tekstowych w Haskellu są o wiele większe. Możemy na przykład wczytać zawartość pliku wiersz po wierszu za pomocą funkcji `readFile` i `lines`:

```Haskell
import System.IO

main = do
    contents <- readFile "plik.txt"
    let lines = splitOn "\n" contents
    mapM_ putStrLn lines
```

Funkcja `splitOn` pochodzi z modułu `Data.List.Split` i dzieli napis na listę w miejscach, gdzie występuje podany separator (w tym wypadku znak nowej linii). Następnie za pomocą funkcji `mapM_` (odpowiednik funkcji `mapM`, ale nie zwraca wyniku) wypisujemy każdy wiersz na ekran.

## Zobacz też

- [Dokumentacja modułu System.IO](http://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Dokumentacja modułu Data.List.Split](http://hackage.haskell.org/package/split-0.2.3.4/docs/Data-List-Split.html)