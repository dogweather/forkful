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

Co i dlaczego?

Tworzenie pliku tymczasowego jest nieodłączną częścią programowania w Haskellu. Jest to plik, który jest tworzony i używany tylko na chwilę, a następnie usuwany. Programiści tworzą tymczasowe pliki w celu przechowywania tymczasowych danych, takich jak kawałki kodu, pliki tymczasowe do przetwarzania, itp.

Jak to zrobić:

Kodowanie w Haskellu jest proste i intuicyjne, a tworzenie plików tymczasowych nie jest wyjątkiem. Użyjemy funkcji ```openTempFile```, która przyjmuje dwa argumenty: ścieżkę do katalogu tymczasowego i nazwę pliku tymczasowego. Oto przykładowe użycie tej funkcji:

```Haskell
import System.IO

main :: IO ()
main = do
  (path, tempFileHandle) <- openTempFile "/tmp" "example.txt"
  hPutStrLn tempFileHandle "To jest przykładowy tekst."
  hClose tempFileHandle
  putStrLn ("Stworzono tymczasowy plik w: " ++ path)
```

Po uruchomieniu tego programu, zobaczysz na ekranie wyjściowym komunikat o stworzeniu tymczasowego pliku w podanym katalogu, a w tym katalogu pojawi się plik "example.txt". Plik ten zostanie automatycznie usunięty po zakończeniu programu.

Głębsze zagadnienia:

Sposoby tworzenia plików tymczasowych w Haskellu mogą się różnić w zależności od używanej biblioteki lub modułu. Inne dostępne funkcje to na przykład ```createTempFile```, ```withTempFile```, czy też ```withSystemTempFile```. Istnieją również alternatywne sposoby umieszczania tymczasowych danych, takie jak korzystanie z strumieni lub innych struktur danych w Haskellu. 

Warto wspomnieć, że plik tymczasowy może również być dostępny w pamięci RAM, a nie tylko na dysku twardym. Wówczas będziemy mówić o "plikach tymczasowych w pamięci". W Haskellu możemy to osiągnąć przy użyciu funkcji ```openTempFile```, podając ścieżkę do katalogu w pamięci.

Zobacz też:

Jeśli chcesz dowiedzieć się więcej o tworzeniu plików tymczasowych w Haskellu, warto zapoznać się z dokumentacją biblioteki ```System.IO```. Możesz również przeczytać o użyciu strumieni lub innych alternatywnych sposobów przechowywania danych w Haskellu.