---
title:                "Haskell: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie tekstowych plików jest nieodłączną częścią wielu projektów programistycznych. Może to pomóc w przechowywaniu danych, tworzeniu plików konfiguracyjnych lub generowaniu raportów. W języku Haskell, istnieje wiele sposobów na tworzenie i manipulowanie plikami tekstowymi, dlatego warto poznać podstawy tego procesu.

## Jak to zrobić

Tworzenie i zapisywanie plików tekstowych w Haskell jest stosunkowo proste, ponieważ język ten posiada wbudowane funkcje do obsługi operacji na plikach. Aby utworzyć nowy plik, należy użyć funkcji "openFile" z biblioteki "System.IO". Przykładowy kod wyglądałby następująco:

```Haskell
import System.IO

main = do
    handle <- openFile "hello.txt" WriteMode
    hPutStrLn handle "Witaj, świecie!"
    hClose handle
```

W powyższym przykładzie, używamy funkcji "openFile" do utworzenia nowego pliku o nazwie "hello.txt" i trybie zapisu. Następnie, za pomocą funkcji "hPutStrLn" zapisujemy do pliku tekst "Witaj, świecie!". Na koniec, używamy funkcji "hClose" aby zamknąć plik. W ten sam sposób można również wczytać dane z pliku za pomocą funkcji "hGetLine" lub "hGetContents".

## Wszystko to i więcej

Ponadto, język Haskell oferuje wiele zaawansowanych funkcji do manipulowania plikami tekstowymi, takich jak zmienianie rozmiaru, usuwanie oraz przesuwanie wskaznika do odczytu/zapisu. Warto również wiedzieć o różnych trybach otwierania pliku, takich jak "ReadMode", "AppendMode" czy "ReadWriteMode". Dokładne informacje na ten temat można znaleźć w dokumentacji języka Haskell.

## Zobacz również

- [Oficjalna dokumentacja języka Haskell](https://www.haskell.org/documentation/)
- [Poradnik dla początkujących w języku Haskell](https://wiki.haskell.org/Tutorials/Programming_Haskell)
- [Kurs programowania w języku Haskell](https://www.edx.org/course/introduction-to-functional-programming-2)