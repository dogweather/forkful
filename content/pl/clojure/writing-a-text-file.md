---
title:                "Tworzenie pliku tekstowego"
html_title:           "Clojure: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Pisanie pliku tekstowego w Clojure oznacza utworzenie pliku zawierającego tekst, który może być odczytany przez użytkownika. Programiści często piszą pliki tekstowe, gdy potrzebują przechowywać dane lub wyniki swojego programu w formacie, który łatwo jest przetworzyć i odczytać.

## Jak to zrobić:

Poniżej znajdują się przykładowe kody w języku Clojure, które pokażą Ci, jak napisać plik tekstowy i odczytać jego zawartość.

```Clojure
(with-open [file (clojure.java.io/writer "plik.txt")]
  (.write file "To jest tekst, który zostanie zapisany w pliku tekstowym!"))

(with-open [file (clojure.java.io/reader "plik.txt")]
  (doseq [line (line-seq file)]
    (println line)))

```

Pierwszy przykład tworzy plik tekstowy o nazwie "plik.txt" i zapisuje w nim zawartość. Drugi przykład odczytuje zawartość pliku i wypisuje ją na ekranie.

## Głębszy zanurzenie:

Pisanie plików tekstowych jest istotną częścią wielu programów. Wcześniej, używano do tego celu różnych języków programowania takich jak Basic czy Pascal. Obecnie, większość języków programowania, w tym Clojure, oferują wbudowane funkcje do obsługi plików tekstowych.
Innymi alternatywami dla zapisywania plików tekstowych są m.in. bazy danych czy formaty danych takie jak CSV czy JSON.

## Zobacz także:

- Dokumentacja języka Clojure: https://clojure.org/guides/io
- Przykładowy kod na GitHubie: https://github.com/clojure-cookbook/clojure-cookbook/blob/master/08_files_and_directories/8-6_write-a-text-file.asciidoc