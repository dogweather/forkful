---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Haskell: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli kiedykolwiek pracowałeś z plikami i musiałeś je tworzyć lub edytować w swoim programie, na pewno spotkałeś się z problemem, że chcesz zachować oryginalną wersję pliku, ale jednocześnie potrzebujesz wykonać na nim jakieś zmiany. Tutaj wchodzi w grę tworzenie tymczasowych plików. Te pliki są stworzone tylko w celach tymczasowych i po zakończeniu pracy z nimi są usuwane automatycznie. W tej krótkiej instrukcji dowiesz się jak stworzyć tymczasowy plik w języku Haskell.

## Jak to zrobić

Aby stworzyć tymczasowy plik w Haskellu, musimy wykorzystać moduł System.IO.Temp i jego funkcję createTempFile. Najpierw musimy zaimportować ten moduł do naszego kodu za pomocą następującego polecenia:

```Haskell
import System.IO.Temp
```

Następnie, aby stworzyć tymczasowy plik, musimy podać ścieżkę do katalogu, w którym będzie przechowywany nasz plik oraz prefiks nazwy pliku. Na przykład, jeśli chcemy utworzyć plik o nazwie "tmpfile", który będzie przechowywany w bieżącym katalogu, możemy to zrobić w następujący sposób:

```Haskell
createTempFile "." "tmpfile"
```

Wynikiem wywołania tej funkcji będzie IO akcja zwracająca FilePath, czyli ścieżkę do naszego nowego tymczasowego pliku. Możemy teraz dalej pracować ze stworzonym plikiem, np. otwierać go i dokonywać na nim zmian.

```Haskell
do
  tempFile <- createTempFile "." "tmpfile"
  putStrLn tempFile    -- wyświetli ścieżkę naszego nowego tymczasowego pliku
```

Po zakończeniu pracy z plikiem, możemy go usunąć za pomocą funkcji removeFile, przyjmując stworzoną ścieżkę jako argument:

```Haskell
removeFile tempFile    -- usuwa nasz tymczasowy plik
```

## Głębsza analiza

Tworzenie tymczasowego pliku jest szczególnie przydatne, gdy pracujemy z niektórymi typami plików, np. konfiguracyjnych, i chcemy zachować oryginalną wersję w celu ewentualnego przywrócenia go. Użycie funkcji createTempFile zapewnia nam, że nie nadpiszemy przypadkowo istniejącego pliku i nie utracimy go w procesie pracy.

Dodatkowo, moduł System.IO.Temp dostarcza także inne funkcje do pracy z tymczasowymi plikami, takie jak withSystemTempFile czy withTempDirectory, które pozwalają nam wykonać operacje na pliku lub katalogu, a następnie usunąć je automatycznie po zakończeniu.

## Zobacz również

- [Dokumentacja modułu System.IO.Temp](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [Tutorial dla początkujących w języku Haskell](https://wiki.haskell.org/Tutorials/Programming_Haskell/Beginners)