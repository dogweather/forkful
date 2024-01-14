---
title:                "Clojure: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczenie daty w przyszłości lub przeszłości może być niezbędne przy tworzeniu aplikacji związanych z datami lub wydarzeniami. Jest to również przydatne, gdy chcemy przewidzieć datę urodzin lub ważnej rocznicy.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w Clojure, musimy użyć funkcji `java.util.Calendar`. Najpierw musimy zaimportować tę funkcję:

```Clojure
(import [java.util Calendar])
```

Aby uzyskać datę w przyszłości, możemy użyć funkcji `add` z podaniem odpowiedniego pola i liczby, jak poniżej:

```Clojure
(def date (Calendar/getInstance))
(.add date Calendar/DAY_OF_MONTH 7)
```

W powyższym przykładzie obliczamy datę, która jest 7 dni później od bieżącej daty.

Aby uzyskać datę w przeszłości, musimy zmienić znak liczby, np. jeśli chcemy uzyskać datę sprzed 7 dni, funkcja `add` wyglądałaby następująco:

```Clojure
(def date (Calendar/getInstance))
(.add date Calendar/DAY_OF_MONTH -7)
```

Po obliczeniu daty, możemy pobrać ją przy użyciu funkcji `get` z podaniem odpowiedniego pola, na przykład:

```Clojure
(println (.get date Calendar/DAY_OF_MONTH) "/" (.get date Calendar/MONTH) "/" (.get date Calendar/YEAR))
```

W wyniku otrzymamy datę w formacie `dzień/miesiąc/rok`.

## Głębszy wgląd

Aby lepiej zrozumieć działanie funkcji `add`, warto przestudiować dokumentację funkcji `Calendar` oraz pola, takie jak `DAY_OF_MONTH` czy `MONTH`. Możemy również zapoznać się z innymi funkcjami i narzędziami dostępnymi w Clojure do pracy z datami, na przykład biblioteką `clj-time` lub funkcją `LocalDate`.

## Zobacz także

- [Dokumentacja funkcji Calendar](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html)
- [Dokumentacja pola DAY_OF_MONTH](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html#DAY_OF_MONTH)
- [Biblioteka clj-time](https://github.com/clj-time/clj-time)
- [Funkcja LocalDate w Java](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)