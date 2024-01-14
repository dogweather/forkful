---
title:                "Clojure: Tworzenie pliku tymczasowego"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się potrzebować tymczasowego pliku w swoim programie Clojure? Istnieje wiele powodów, dla których warto stworzyć taki plik, na przykład do tymczasowego przechowywania danych lub tworzenia kopii zapasowych. W tym artykule dowiesz się, jak w prosty sposób stworzyć tymczasowy plik w Clojure.

## Jak to zrobić

Tworzenie tymczasowego pliku w Clojure jest bardzo proste. Wystarczy, że użyjesz funkcji `tempfile`, która wraz z funkcją `with-open` pozwala na otwarcie pliku w trybie do odczytu i zapisu. Poniżej znajduje się przykładowy kod, który to demonstruje:

```Clojure
(require '[clojure.java.io :as io])
(with-open [file (io/tempfile "temp")]
  (println "Plik tymczasowy został stworzony pod ścieżką:" (.getAbsolutePath file)))
```

Po wykonaniu tego kodu, pod ścieżką zostanie utworzony tymczasowy plik o nazwie "temp". Pamiętaj, że zawsze należy używać funkcji `with-open`, aby upewnić się, że plik zostanie poprawnie zamknięty po zakończeniu jego użycia.

## Głębsze zanurzenie

Funkcje `tempfile` i `with-open` są tylko jednymi z wielu dostępnych w Clojure, które pozwalają na operacje na plikach. Możesz także skorzystać z funkcji `with-out-str`, aby zapisać dane do tymczasowego strumienia wyjściowego, lub użyć biblioteki `fressian` do zapisu i odczytu danych w formie binarnej.

Zawsze pamiętaj, że tworzenie tymczasowych plików jest pomocne, ale należy pamiętać o ich usunięciu po zakończeniu ich użycia. W przypadku gdy potrzebujesz tymczasowego pliku tylko do odczytu danych, możesz skorzystać z funkcji `tmpfile-seq`, która automatycznie usuwa pliki po zakończeniu ich użycia.

## Zobacz też

- [Dokumentacja Clojure](https://clojure.org)
- [Poradniki Clojure od podstaw](https://clojure.org/guides/getting_started)
- [Kurs Clojure dla początkujących](https://learnxinyminutes.com/docs/pl-pl/clojure-pl)