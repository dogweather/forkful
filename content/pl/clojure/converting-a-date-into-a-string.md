---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Clojure: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest powszechnym zadaniem, które może być niezbędne w wielu projektach. W tym artykule dowiesz się, jak w prosty sposób przekształcić datę do formatu, który najlepiej odpowiada Twoim potrzebom.

## Jak to zrobić

Najpierw musimy skonwertować datę na obiekt typu `java.util.Date`. Możemy to zrobić za pomocą funkcji `date` lub `now` z biblioteki `java-time`. Następnie używamy funkcji `format` z tej samej biblioteki, podając jako argumenty format daty i obiekt `Date`. Oto przykładowy kod:

```Clojure
(require '[java-time :as jt])

(def date (jt/date 2020 6 15))
(jt/format "dd.MM.yyyy" date)
;; wynik: "15.06.2020"
```

Możemy również użyć składni `#inst` do utworzenia obiektu `Date`. W poniższym przykładzie użyjemy formatu `yyyy-MM-dd`, który jest często wykorzystywany w bazach danych:

```Clojure
(def date #inst "2020-06-15")
(jt/format "dd/MM/yyyy" date)
;; wynik: "15/06/2020"
```

Jeśli chcemy, aby nasz ciąg znaków zawierał również informację o godzinie, możemy użyć formatu `yyyy-MM-dd HH:mm:ss`:

```Clojure
(def date #inst "2020-06-15T14:30:00")
(jt/format "dd/MM/yyyy HH:mm:ss" date)
;; wynik: "15/06/2020 14:30:00"
```

Oczywiście istnieje wiele innych formatów, których możemy użyć do konwersji daty na ciąg znaków. Dokładny opis dostępnych opcji znajduje się w dokumentacji biblioteki `java-time`.

## Deep Dive

Podczas korzystania z funkcji `format`, możemy natknąć się na kilka nieoczekiwanych błędów lub niezrozumiałych wyników. Sprawdźmy kilka z nich i ich możliwe przyczyny.

Błędy związane z formatowaniem daty mogą wynikać z niepoprawnego użycia formatu. W przypadku formatów ze znakami specjalnymi, takimi jak `:` lub `/`, musimy użyć cudzysłowów lub znaku ucieczki `\`. Na przykład, jeśli chcemy ustawić format jako `HH:mm:ss`, musimy użyć cudzysłowów lub `\\`:

```Clojure
(jt/format "HH\\:mm\\:ss" date)
```

Kolejnym możliwym problemem może być złe ustawienie strefy czasowej. Domyślnie funkcja `format` korzysta z aktualnej strefy czasowej, ale możemy ją zmienić za pomocą funkcji `with-time-zone`. Na przykład, jeśli chcemy uzyskać datę w czasie UTC, musimy dodać `jt/with-time-zone UTC` przed funkcją `format`:

```Clojure
(jt/with-time-zone jt/UTC
  (jt/format "dd/MM/yyyy HH:mm:ss" date))
;; wynik: "15/06/2020 12:30:00"
```

## Zobacz także

- [Dokumentacja biblioteki `java-time`](https://github.com/dm3/clojure.java-time)
- [Artykuł o formatowaniu dat w Clojure](https://medium.com/technical-credit/clojure-conversations-dates-and-times-7070c2a6428f)
- [Dokumentacja `java.text.SimpleDateFormat` zawierająca listę dostępnych formatów](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)