---
title:                "Clojure: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego może być przydatne, gdy potrzebujemy wygenerować dane lub przeprowadzić operacje na pliku, ale nie chcemy pozostawiać go na stałe w naszym systemie. Może to być szczególnie pomocne podczas testowania kodu lub w sytuacji, gdy mamy do czynienia z ograniczoną przestrzenią dyskową.

## Jak to zrobić

W Clojure możemy łatwo utworzyć plik tymczasowy za pomocą funkcji `with-open` i `TemporaryFileWriter`. Oto przykładowy kod:

```Clojure
(with-open [file (java.io.File/createTempFile "temp" ".txt")]
  (let [writer (clojure.java.io/writer file)]
    (.write writer "Hello World!")
    (.flush writer)))
```

W tym przykładzie najpierw tworzymy plik tymczasowy o nazwie "temp" i rozszerzeniu ".txt". Następnie używamy funkcji `writer` z modułu `clojure.java.io`, aby utworzyć obiekt do zapisu danych do naszego pliku. W końcu zapisujemy naszą wiadomość i zamykamy plik. Dzięki użyciu `with-open`, plik zostanie automatycznie usunięty po zakończeniu tego bloku kodu.

Warto również pamiętać, że można używać różnych funkcji z modułu `clojure.java.io` do operacji na plikach, takich jak czytanie lub usuwanie.

## Dogłębna analiza

Tworzenie pliku tymczasowego jest zdecydowanie prostszym i bezpieczniejszym sposobem na tymczasowe zapisywanie danych niż ręczne tworzenie i usuwanie plików. Ponadto funkcje z modułu `clojure.java.io` są zgodne z interfejsem Javy, więc możemy używać ich w połączeniu z innymi językami programowania.

Jednak warto pamiętać, że plik tymczasowy jest traktowany przez system operacyjny jako normalny plik, więc może wystąpić konflikt nazw, jeśli użyjemy tej samej nazwy kilka razy w różnych częściach naszego kodu. Dlatego dobrą praktyką jest generowanie różnych i unikalnych nazw dla plików tymczasowych.

## Zobacz również

- [Dokumentacja funkcji with-open w Clojure](https://clojuredocs.org/clojure.java.io/with-open)
- [Sekcja Java Interop w praktyce w oficjalnym tutorialu Clojure](https://clojure.org/guides/interop_in_depth)
- [Tutorial na temat operacji na plikach w Clojure](https://yobriefca.se/blog/2014/07/15/file-handling-reading-writing-and-appending-with-clojure/)