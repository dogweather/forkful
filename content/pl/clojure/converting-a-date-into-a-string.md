---
title:    "Clojure: Konwersja daty na ciąg znaków"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Często podczas programowania potrzebujemy konwertować datę na łańcuch znaków. W ten sposób można wyświetlić datę w bardziej przyjaznej dla użytkownika formie, na przykład w formacie "dzień-miesiąc-rok".

## Jak to zrobić
```Clojure
(ns konwertowanie-daty-string.core
  (:require [clj-time.core :as t]))

(defn konwertuj-date [data]
  (t/format data "dd-MM-yyyy"))

(prn (konwertuj-date (t/date-time 2020 10 2)))
;; Output: "02-10-2020"
```

W powyższym przykładzie korzystamy z biblioteki `clj-time`, która zapewnia pomocne funkcje do pracy z datami w języku Clojure. Po zadeklarowaniu namespace'u i wymaganiu biblioteki, definiujemy funkcję `konwertuj-date`, która przyjmuje datę, a następnie wykorzystuje funkcję `format` do przekształcenia jej na łańcuch znaków w wybranym formacie. W przykładzie używamy tu formatu "dd-MM-yyyy", ale możliwe jest również ustalenie innych formatów, takich jak "yyyy-MM-dd" czy "dd MMMM yyyy". Konwertowana data jest przekazana jako argument do funkcji `prn` (wypisującej na konsolę), a następnie wyświetlana jest łańcuch znaków.

## Głębsza analiza
W języku Clojure istnieje wiele sposobów na konwersję daty do łańcucha znaków. Można wykorzystać funkcję `format` z biblioteki `clj-time`, ale możliwe jest również użycie standardowej funkcji `str` lub `format` z biblioteki `clojure.core`. Przykładem takiej konwersji może być:

```Clojure
(ns konwertowanie-daty-string.core)

(defn konwertuj-date [data]
  (str (.getDayOfMonth data) "-"
       (.getMonth data) "-"
       (.getYear data)))

(prn (konwertuj-date (java.util.Date.)))
;; Output: "3-10-2020"
```

Jeśli chcemy konwertować datę z bazy danych, warto skorzystać z biblioteki `java-time`, która pozwala na wygodne operowanie datami i czasami w języku Clojure. Przykład konwersji może wyglądać tak:

```Clojure
(ns konwertowanie-daty-string.core
  (:require [java-time :refer [local-date format]]))

(defn konwertuj-date [data]
  (format (local-date data) "dd-MM-yyyy"))

(prn (konwertuj-date "2020-10-02"))
;; Output: "02-10-2020"
```

W przypadku, gdy potrzebujemy bardziej zaawansowanych operacji na konwertowanej dacie, warto rozważyć użycie biblioteki `clj-time.java-time`, która łączy funkcjonalność bibliotek `clj-time` i `java-time`.

## Zobacz także
- Dokumentacja biblioteki `clj-time`: https://github.com/clj-time/clj-time
- Dokumentacja funkcji `str`: https://clojuredocs.org/clojure.core/str
- Dokumentacja funkcji `format` z biblioteki `clojure.core`: https://clojuredocs.org/clojure.core/format
- Dokumentacja biblioteki `java-time`: https://github.com/dm3/clojure.java-time