---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja daty na łańcuch znaków to proces przekształcania informacji o dacie z postaci numerycznej na czytelny dla człowieka tekst. Programiści robią to, aby ułatwić prezentację daty użytkownikom, logom czy systemom docelowym.

## Jak to zrobić:

Clojure udostępnia funkcję `format` z biblioteki `java.time.format.DateTimeFormatter`. Przykładowe użycie poniżej.

```Clojure
(import java.time.LocalDate)
(import java.time.format.DateTimeFormatter)

(def d (java.time.LocalDate/of 2020 12 31))

(defn date-to-str [date]
  (.format date (java.time.format.DateTimeFormatter/ofPattern "dd-MM-yyyy")))
  
(println (date-to-str d)) ; Wypisuje "31-12-2020"
```

## Dogłębne informacje:

### Kontekst historyczny

Java dostarcza mocne wsparcie dla dat i czasu od początku swej historii. Zatem Clojure, który działa na JVM (Java Virtual Machine), również korzysta z tych możliwości. 

### Alternatywy

Dla prostszych formatów daty możemy użyć także funkcji `str`.

```Clojure
(def d2 (java.util.Date.))
(println (str d2))
```

### Szczegóły implementacji

Funkcja `format` korzysta z `java.time.format.DateTimeFormatter`, który jest niemutowalny i bezpieczny do używania w środowisku wielowątkowym. 

## Zobacz również:

- Clojure - Working with Dates and Times: https://clojurecookbook.com/clojure_dates_and_times/dates_and_times/
- Java 8 – How to format LocalDat: https://mkyong.com/java8/java-8-how-to-format-localdate/
- Java Time API: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html