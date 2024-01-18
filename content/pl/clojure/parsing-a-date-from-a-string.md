---
title:                "Analiza daty z ciągu znaków"
html_title:           "Clojure: Analiza daty z ciągu znaków"
simple_title:         "Analiza daty z ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Co i po co parsowanie daty ze stringa?

Parsowanie daty ze stringa, to proces zamiany daty zapisanej w formacie tekstowym, na format daty, który może być dalej przetwarzany przez program. Programiści często wykonują tę operację, kiedy muszą przetwarzać wejściowe dane zawierające informację o dacie, lub kiedy chcą wyświetlić wynik w czytelniejszy sposób.

Jak to zrobić:

```Clojure
(require '[clojure.java-time :as time])

(time/parse "11/12/2021" "M/d/yyyy")
;; => #object[java.time.LocalDate 0x17d2420e "2021-11-12"]
```

Głębsze spojrzenie:

Parsowanie daty ze stringa jest często potrzebne, ponieważ wiele programów przetwarza dane zawierające informację o dacie, np. raporty finansowe, statystyki, czy albumy zdjeciowe. W przeszłości programiści musieli ręcznie pisać funkcje do parsowania dat, ale dzięki bibliotece clojure.java-time, proces ten jest znacznie ułatwiony.

Alternatywą dla biblioteki clojure.java-time jest biblioteka clojure.tools.timeline, która także udostępnia funkcję do parsowania dat. Jednak clojure.java-time jest zalecana, ponieważ jest oficjalną wersją biblioteki języka Clojure.

Parsowanie daty ze stringa jest realizowane w bibiliotece clojure.java-time przez wykorzystanie metody parse z klasy java.time.format.DateTimeFormatter. Dzięki temu, mamy do dyspozycji różne formaty daty, które mogą być dostosowane do naszych potrzeb.

Zobacz także:

https://clojure.github.io/java-time/
https://github.com/clojure/java-time