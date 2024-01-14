---
title:                "Clojure: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest ważnym elementem w tworzeniu oprogramowania w języku Clojure. Umożliwia to komunikację z użytkownikami poprzez wyświetlanie treści w przejrzysty i interaktywny sposób. W tym poście dowiesz się, dlaczego warto nauczyć się pisać pliki tekstowe w Clojure.

## Jak to zrobić

Pisanie plików tekstowych w Clojure jest łatwe i przyjemne. Aby rozpocząć, musisz najpierw zaimportować bibliotekę `clojure.java.io`, która udostępnia funkcje do pracy z plikami. Następnie możesz utworzyć nowy plik za pomocą funkcji `open-writer`, określając jego nazwę i tryb (nadpisywania lub dopisywania). W celu zapisania tekstu do pliku, możesz użyć funkcji `write` lub `println`, a na koniec zamknąć plik za pomocą `close`.

```Clojure  
(require '[clojure.java.io :as io])

(def plik (io/open-writer "moj_plik.txt" :append))

(io/write plik "Witaj, to jest mój pierwszy plik tekstowy w Clojure!")
(io/println plik "Zapisuję kolejną linię tekstu w pliku.")
(io/close plik)
```

Po wykonaniu powyższego kodu, plik `moj_plik.txt` powinien zawierać następujące linie:

`Witaj, to jest mój pierwszy plik tekstowy w Clojure!`
`Zapisuję kolejną linię tekstu w pliku.`

## Głębsza analiza

Tworzenie plików tekstowych w Clojure może być bardziej zaawansowane dzięki użyciu biblioteki `clojure.data.csv`. Umożliwia ona pracę z danymi w formacie CSV, co jest szczególnie przydatne przy tworzeniu raportów lub eksportowaniu danych z bazy. Możesz też skorzystać z funkcji `spit` lub `slurp` w celu szybkiego zapisu lub odczytu całych plików tekstowych.

```Clojure  
(use '[clojure.data.csv :only (write-csv)])

(def dane [["Imię" "Nazwisko" "Wiek"]
           ["Anna" "Kowalska" 25]
           ["Jan" "Nowak" 32]])

(write-csv "dane.csv" dane)

(def odczytane-dane (slurp "dane.csv"))

(println odczytane-dane)
```

Powyższy kod zapisze do pliku `dane.csv` dane w postaci tabeli CSV, a następnie odczyta je z pliku i wyświetli na ekranie:

```
Imię,Nazwisko,Wiek
Anna,Kowalska,25
Jan,Nowak,32
```

## Zobacz również

- Dokumentacja biblioteki `clojure.java.io`: https://clojure.github.io/clojure/clojure.java.io-api.html
- Dokumentacja biblioteki `clojure.data.csv`: https://github.com/clojure/data.csv
- Przydatne poradniki i tutoriale dotyczące tworzenia aplikacji w Clojure: https://clojure.org/guides/getting_started