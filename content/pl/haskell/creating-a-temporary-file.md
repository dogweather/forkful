---
title:                "Haskell: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest nieodłączną częścią programowania w Haskellu. Wiele zadań wymaga tymczasowego przechowywania danych lub plików, które nie są potrzebne w dłuższym horyzoncie. Tworzenie tymczasowych plików jest szybkie, łatwe i efektywne, dzięki czemu jest idealnym wyborem dla wielu zadaniom.

## Jak to zrobić

Aby stworzyć tymczasowy plik w Haskellu, należy skorzystać z funkcji `withSystemTempFile` z pakietu `System.IO.Temp`. Przykładowy kod wygląda następująco:

```Haskell
import System.IO.Temp (withSystemTempFile)

main :: IO ()
main = do
    withSystemTempFile "mojplik.txt" $ \path handle -> do
        putStrLn $ "Utworzono tymczasowy plik: " ++ path
        hPutStrLn handle "To jest zawartość pliku."
```

Powyższy kod otwiera tymczasowy plik o nazwie `mojplik.txt` i zapisuje do niego linię tekstu. Następnie, funkcja `withSystemTempFile` automatycznie usuwa stworzony plik po zakończeniu działania programu.

Kod można również rozszerzyć o wczytywanie danych z innych plików czy wykonanie innych operacji na tymczasowych plikach.

## Dogłębne wrażenie

Tworzenie tymczasowych plików jest proste, ale warto pamiętać, że nie są one domyślnie usuwane po zakończeniu programu. Warto więc zadbać o odpowiednie czyszczenie wykorzystanych plików, aby nie zajmowały one niepotrzebnie miejsca.

## Zobacz również

- [Dokumentacja funkcji `withSystemTempFile`](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html#v:withSystemTempFile)
- [Tutorial o tworzeniu i zamykaniu tymczasowych plików w Haskellu](https://wiki.haskell.org/How_to_create_a_temporary_file)
- [Inne przydatne funkcje z pakietu `System.IO.Temp`](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)