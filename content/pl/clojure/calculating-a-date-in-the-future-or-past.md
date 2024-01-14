---
title:    "Clojure: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być istotne w wielu programach, szczególnie w tych, które wymagają określania okresów czasu. Na przykład, może to dotyczyć programów rezerwacji biletów lub kalendarzy. W Clojure możesz użyć wbudowanych funkcji, aby łatwo i precyzyjnie obliczyć daty w przyszłości lub przeszłości.

## Jak to zrobić

Obliczenie daty w przyszłości lub przeszłości w Clojure jest łatwe i wymaga kilku kroków. Najpierw musisz zaimportować bibliotekę "clojure.java.time", zawierającą wiele przydatnych funkcji do pracy z datami. Następnie, używając funkcji "plus" lub "minus", możesz wybrać, czy chcesz dodać lub odjąć określony czas od bieżącej daty. Możesz również określić jednostkę czasu, np. "days" lub "months", aby uzyskać bardziej precyzyjny wynik.

```Clojure
(require '[clojure.java.time :as time])

; Dodanie 7 dni do bieżącej daty
(time/plus (java.time.LocalDate/now) 7 :days)

; Odjęcie 1 miesiąca od bieżącej daty
(time/minus (java.time.LocalDate/now) 1 :months)
```

W powyższym przykładzie użyto funkcji "now" do pobrania bieżącej daty. Możesz również podać własną datę jako argument. Funkcja "plus" i "minus" zwraca datę w postaci obiektu Java, więc jeśli chcesz ją sformatować, musisz użyć funkcji "format" z biblioteki "clojure.string".

```Clojure
; Ustawienie własnej daty jako argumentu
(def start-date (java.time.LocalDate/of 2021 1 1))

(time/plus start-date 1 :years)

; Sformatowanie wyniku
(clojure.string/format "%1$td-%1$tm-%1$tY" (time/plus start-date 1 :years))
```

Wynik powinien wyglądać następująco: "01-01-2022".

## Głębszy kontekst

Istnieje również możliwość obliczenia daty w przyszłości lub przeszłości z uwzględnieniem różnych stref czasowych. W tym celu możesz użyć funkcji "plus-hours", "plus-days", itp. z biblioteki "clojure.java.time". Te funkcje możliwe do wykorzystania również pozwalają na przekazywanie obiektów dat z innych stref czasowych.

```Clojure
; Obliczanie daty w przyszłości z uwzględnieniem różnych stref czasowych
(def date-now (java.time.Instant/now))

; Przekazanie obiektu daty z innej strefy czasowej
(def date-tokyo (java.time.ZoneId/of "Asia/Tokyo").from date-now)

(time/plus-hours date-tokyo 12)
```

W powyższym przykładzie wynik wynosiłby datę 12 godzin później, ale w strefie czasowej Tokio.

## Zobacz również

- Dokumentacja Clojure na temat dat: https://clojure.org/api/java.time
- Przewodnik po pracy z datami w Clojure: https://purelyfunctional.tv/guide/date-time/