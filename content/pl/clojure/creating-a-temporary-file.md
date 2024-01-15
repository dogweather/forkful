---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Clojure: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest nieodłączną częścią procesu tworzenia oprogramowania. Często potrzebujemy tymczasowego miejsca na przechowywanie danych, które będą użyte tylko w określonym momencie, bez konieczności trwałego zapisywania ich na dysku. W takich przypadkach tworzenie plików tymczasowych może okazać się bardzo przydatne.

## Jak to zrobić

W Clojure możemy łatwo utworzyć pliki tymczasowe przy użyciu funkcji `with-open` i `java.io.File/createTempFile`. Przykładowy kod wyglądałby następująco:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
  (println (.getName temp-file))) ; wyświetli nazwę tymczasowego pliku, np. "temp12345.txt"
```

W powyższym przykładzie tworzymy plik tymczasowy o nazwie "temp12345.txt". Jest on automatycznie usuwany po wyjściu z bloku `with-open`. Ponadto, możemy wewnątrz bloku wykonać dowolne operacje na pliku, np. jego zapis lub odczyt danych.

## Dogłębna analiza

Funkcja `java.io.File/createTempFile` przyjmuje dwa argumenty: prefiks i sufiks dla nazwy tymczasowego pliku. Jest również dostępna opcjonalna trzecia wartość, która może określić katalog, w którym ma zostać utworzony plik tymczasowy. Domyślnie plik jest tworzony w katalogu systemowym przeznaczonym na tymczasowe pliki.

Funkcja `with-open` jest szczególnie przydatna, ponieważ dba o zamknięcie pliku po zakończeniu wykonywania bloku kodu. Dzięki temu nie musimy martwić się o ręczne zamknięcie pliku.

## Zobacz także

- Dokumentacja funkcji `java.io.File/createTempFile`: https://clojuredocs.org/clojure.java.io/file/createTempFile
- Dokumentacja funkcji `with-open`: https://clojuredocs.org/clojure.core/with-open