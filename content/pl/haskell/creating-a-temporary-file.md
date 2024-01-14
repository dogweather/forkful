---
title:    "Haskell: Tworzenie pliku tymczasowego"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest nieuniknionym aspektem programowania. Czasem niezbędne jest tymczasowe przechowywanie danych lub dokumentów. W programowaniu w języku Haskell tworzenie tymczasowych plików może być szczególnie przydatne przy pracach na dużych i złożonych projektach.

## Jak to zrobić

Aby stworzyć tymczasowy plik w Haskellu, możesz skorzystać z funkcji `withSystemTempFile`. Poniżej przedstawiam przykładowy kod z wykorzystaniem tej funkcji oraz wypisaniem zawartości pliku:

```Haskell
import System.IO
import System.IO.Temp

main = withSystemTempFile "sample.txt" $ \tmpFilePath tmpFileHandle -> do
  hPutStrLn tmpFileHandle "To jest przykładowy tekst."
  hSeek tmpFileHandle AbsoluteSeek 0
  contents <- hGetContents tmpFileHandle
  putStrLn contents
```

Po uruchomieniu tego kodu, w katalogu tymczasowym zostanie utworzony plik "sample.txt" zawierający tekst "To jest przykładowy tekst." Następnie program wypisze tę treść na ekranie. 

## Głębsze spojrzenie

Funkcja `withSystemTempFile` przyjmuje dwa argumenty: nazwę pliku oraz funkcję działającą na ścieżce do tymczasowego pliku oraz uchwycie do niego. Funkcja ta automatycznie usuwa plik po jej wykonaniu.

Poza funkcją `withSystemTempFile`, w Haskellu istnieje również funkcja `writeSystemTempFile`, która tworzy tymczasowy plik i zapisuje w nim podany tekst.

Tworzenie tymczasowych plików jest niezwykle przydatne, ale pamiętaj, że są one usuwane automatycznie po wykonaniu programu. Jeśli potrzebujesz przechowywać dane dłużej, należy zapisać je w stałym pliku.

## Zobacz także

- [Dokumentacja Haskella dotycząca tworzenia i przechowywania plików](https://www.haskell.org/documentation/)
- [Przydatne funkcje dla pracy z plikami w Haskellu](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_FileIO)