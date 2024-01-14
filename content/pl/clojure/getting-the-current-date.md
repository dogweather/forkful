---
title:    "Clojure: Otrzymanie bieżącej daty"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach skomputeryzowane systemy są wszechobecne i wiele aplikacji wymaga przetwarzania dat. W Clojure (języku programowania działającym na platformie JVM) pojawia się wiele przypadków, w których potrzebujemy aktualnej daty. W tym artykule dowiecie się, jak to zrobić w kilku prostych krokach.

## Jak to zrobić

Aby uzyskać aktualną datę w Clojure, możemy skorzystać z funkcji `(java.util.Date.)` i `(java.time.LocalDate/now)`. Pierwsza z nich zwróci obiekt typu `java.util.Date`, a druga obiekt typu `java.time.LocalDate`. Przykładowy kod w Clojure wyglądałby następująco:

```
Clojure
(def aktualna-data (java.util.Date.)) ;; aktualna-data = Wed Oct 13 18:57:43 CEST 2021
(def aktualna-data-local (java.time.LocalDate/now)) ;; aktualna-data-local = #object[xxx java.time.LocalDate]  (dokładna wartość zależy od aktualnej daty)
```

Obiekt `aktualna-data` jest typu `java.util.Date`, więc możemy z niego wyodrębnić konkretne informacje, np. dzień, miesiąc czy rok. Inaczej jest w przypadku obiektu `aktualna-data-local`, w którym możemy uzyskać dostęp do poszczególnych wartości używając odpowiednich metod, np. `(.getDayOfMonth aktualna-data-local)` zwróci bieżący dzień miesiąca. Przykładowy kod z wykorzystaniem tych informacji wyglądałby tak:

```
Clojure
(def dzien (.getDay aktualna-data)) ;; dzien = 13
(def miesiac (.getMonth aktualna-data)) ;; miesiac = 10
(def rok (.getYear aktualna-data)) ;; rok = 2021
(def biezacy-dzien-miesiaca (.getDayOfMonth aktualna-data-local)) ;; biezacy-dzien-miesiaca = 13
(def biezacy-miesiac (.getMonth aktualna-data-local)) ;; biezacy-miesiac = OCTOBER
(def biezacy-rok (.getYear aktualna-data-local)) ;; biezacy-rok = 2021
```

## Głębsza analiza

W powyższych przykładach wykorzystaliśmy tylko podstawowe funkcje i metody dostępne w Clojure do uzyskania aktualnej daty. Jednak w zależności od potrzeb, możemy sięgnąć po więcej zaawansowanych rozwiązań, np. biblioteki `clj-time` lub `java.time.format.DateTimeFormatter`, która umożliwia formatowanie daty według własnych potrzeb. Również warto zapoznać się z dokumentacją dostępną na oficjalnej stronie Clojure, aby dowiedzieć się więcej na temat dostępnych funkcji i metod.

## Zobacz także

- Dokumentacja Clojure: https://clojure.org
- Biblioteka clj-time: https://github.com/clj-time/clj-time
- Dokumentacja java.time.format.DateTimeFormatter: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html