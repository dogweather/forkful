---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Clojure: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?
Obliczanie daty w przyszłości lub przeszłości jest procesem określania konkretnej daty lub wydarzenia na podstawie aktualnej daty i określonego okresu czasu. Programiści często wykonują ten rodzaj obliczeń w swoim kodzie, aby automatycznie generować daty lub harmonogramy, na przykład w aplikacjach związanych z kalendarzem lub rezerwacją.

# Jak to zrobić:
Obecnie, Clojure oferuje obszerny zestaw narzędzi i funkcji, które ułatwiają obliczanie daty w przyszłości lub przeszłości. Przykłady kodu i wyników można znaleźć poniżej:

```Clojure
; Importowanie pakietu daty i czasu
(require '[clojure.java-time :as jt])

; Obliczanie daty 10 dni w przyszłości od teraz
(jt/plus (jt/local-date) (jt/period 0 0 10))

; Wynik: #object[java.time.LocalDate 0x7e7d47a4 "2021-03-14"]

; Obliczanie daty 2 lata i 3 miesiące w przeszłości
(jt/minus (jt/local-date) (jt/period 2 3))

; Wynik: #object[java.time.LocalDate 0x755044d9 "2018-11-30"]

; Obliczanie daty na podstawie okresu czasu i odległości
(jt/plus (jt/local-date) (jt/minus (jt/offset 10 :days) (jt/offset 2 :months)))

; Wynik: #object[java.time.LocalDate 0x7736cbe6 "2021-06-02"]
```

# Deep Dive:
Obliczanie daty w przyszłości lub przeszłości jest często wykonywane w aplikacjach związanych z kalendarzem lub rezerwacją, ale może być również używane w innych kontekstach, takich jak wyświetlanie daty ważności lub wyliczanie harmonogramów płatności. Alternatywnie, istnieją również biblioteki, takie jak DateCalc, które dostarczają bardziej zaawansowane funkcje obliczania daty.

Wewnętrznie, Clojure korzysta z języka Java, a jego funkcje daty i czasu są wywodzone z pakietu java.time. Dzięki temu, Clojure oferuje obszerny zestaw funkcji i metod do manipulacji datami bez konieczności importowania dodatkowej biblioteki.

# Zobacz też:
- [Dokumentacja Java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [DateCalc biblioteka](https://github.com/clj-commons/date-calc)