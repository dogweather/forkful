---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie aktualnej daty oznacza uzyskanie informacji o bieżącym dniu, miesiącu i roku z systemu. Programiści robią to, aby rejestrować czas wydarzeń, tworzyć pieczęcie czasowe czy obsługiwać daty i godziny.

## Jak to zrobić:

W Clojure, aby uzyskać aktualną datę, używamy `java.util.Date.` Zobaczmy jak to działa:

```Clojure
(defn get-current-date []
  (java.util.Date.))

(println (get-current-date))
```

Kiedy uruchomisz powyższy kod, zobaczysz aktualną datę i czas, coś w rodzaju:

```Clojure
Tue Sep 14 12:30:29 CEST 2021
```

## Głębsza analiza

Choć `java.util.Date.` jest najprostszym sposobem na pobranie aktualnej daty, istnieją inne metody, które mogą się okazać przydatne. 

Historia: Clojure, będący językiem hostowanym na JVM, dziedziczy wiele funkcji Java, w tym obsługę daty i czasu.

Alternatywy: Możesz użyć java.time API, które jest o wiele bardziej rozbudowane i dokładne, pozwalając na manipulację datą i godziną z większą precyzją. Kod poniżej pokazuje, jak to zrobić:

```Clojure
(defn get-current-date []
  (.toString (java.time.LocalDate/now)))

(println (get-current-date))
```

Szczegóły implementacji: Pobieranie aktualnej daty może być zróżnicowane w zależności od strefy czasowej i ustawień lokalnych. Domyślnie, Clojure używa strefy czasowej i lokalizacji systemu.

##Zobacz również:

Sprawdź dokumentację java.time API: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html

Jeżeli chcesz dowiedzieć się więcej o dacie i czasie w Clojure, odwiedź: https://clojuredocs.org/clojure.core/date

Blog o obsłudze daty i czasu w Clojure: https://www.baeldung.com/clojure-date-time-handling.