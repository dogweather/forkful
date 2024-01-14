---
title:                "Clojure: Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego
Czasami może zdarzyć się, że musimy obliczyć datę w przeszłości lub w przyszłości. Może to być potrzebne w skrypcie rezerwacji biletów lotniczych lub podczas planowania wyjazdu. W tym artykule pokażemy, jak w łatwy sposób obliczyć datę w przód lub w tył za pomocą języka Clojure.

## Jak to zrobić
Pierwszym krokiem jest zaimportowanie biblioteki `java.time.LocalDate`, która umożliwia operacje na datach. Następnie, używając funkcji `now`, możemy pobrać aktualną datę. Aby obliczyć datę w przyszłości lub przeszłości, należy użyć funkcji `plusDays` lub `minusDays` i podać jako argument liczbę dni, o jaką chcemy przesunąć datę.

```Clojure
(import java.time.LocalDate)

;; Obliczenie daty za 30 dni w przód
(println (plusDays (now) 30))

;; Obliczenie daty za 14 dni w tył
(println (minusDays (now) 14))

```

Kod powyżej zwróci odpowiednio daty za 30 dni w przód i za 14 dni w tył. W ten sam sposób można obliczyć również datę z przesunięciem o tygodnie, miesiące lub lata korzystając z funkcji `plusWeeks`, `plusMonths` lub `plusYears`.

## Głębszy wgląd
Biblioteka `java.time.LocalDate` oferuje również inne przydatne funkcje, takie jak porównywanie dat, obliczanie różnicy między dwoma datami czy konwersja daty na inny format. Więcej szczegółowych informacji na temat operacji na datach można znaleźć w oficjalnej dokumentacji języka Clojure.

## Zobacz również
- [Dokumentacja biblioteki java.time w Clojure](https://clojure.github.io/java-time/)
- [Oficjalna strona języka Clojure](https://clojure.org/)