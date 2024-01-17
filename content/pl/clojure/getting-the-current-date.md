---
title:                "Pobieranie aktualnej daty"
html_title:           "Clojure: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Pobranie aktualnej daty w języku Clojure jest proste i przydatne dla programistów. Pozwala to na uzyskanie informacji o aktualnym czasie, co jest niezbędne przy tworzeniu aplikacji związanego z czasem, takich jak kalendarze czy notatniki.

## Jak to zrobić:

```Clojure
(ns current-date.example
  (:require [java.time.LocalDate :as ld]
  [java.time.LocalTime :as lt]))
  
;; pobranie aktualnej daty
(def current-date (ld/now))

;; pobranie aktualnego czasu
(def current-time (lt/now))

;; pobranie aktualnej daty i czasu
(def current-date-time (lt/combine current-date current-time))

;; wyświetlenie aktualnej daty i czasu w formacie yyyy/MM/dd HH:mm:ss
(println (str "Aktualna data i czas: " (ld/format current-date-time "yyyy/MM/dd HH:mm:ss")))
```

Output:
```
Aktualna data i czas: 2020/12/04 11:59:50
```

## Głębsze zagadnienia:

Funkcja do pobierania aktualnej daty w języku Clojure jest oparta na bibliotece Java Time, która została wprowadzona w wersji 1.8. Dzięki temu za pomocą wbudowanych funkcji można łatwo manipulować i przetwarzać daty.

Alternatywą dla użycia biblioteki Java Time jest biblioteka clj-time, która oferuje bardziej czytelne i wygodniejsze funkcje do pracy z datami.

## Zobacz też:

- [Dokumentacja biblioteki Java Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Strona projektu biblioteki clj-time](https://github.com/duck1123/clj-time)