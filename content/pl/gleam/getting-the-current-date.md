---
title:                "Gleam: Otrzymywanie bieżącej daty"
simple_title:         "Otrzymywanie bieżącej daty"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie bieżącej daty jest ważnym aspektem programowania w Gleam. Dzięki temu możemy wyświetlać aktualny czas na naszych stronach internetowych, tworzyć polecenia w grach lub sprawdzać datę ważności w aplikacjach.

## Jak to zrobić

Poniżej znajdziesz przykłady kodu, które pomogą Ci w pobieraniu bieżącej daty w języku programowania Gleam.

```Gleam
use time

current_date = time.now()

// Wyświetlanie bieżącej daty
IO.print("Bieżąca data to: ", current_date)
```

Wynik:
`Bieżąca data to: 2021-10-13T14:30:00Z`

```Gleam
// Pobranie bieżącego roku
current_year = current_date.year()

// Pobranie bieżącego miesiąca
current_month = current_date.month()

// Pobranie bieżącego dnia
current_day = current_date.day()

// Wyświetlanie bieżącej daty w formacie DD.MM.YYYY
IO.print("#{current_day}.#{current_month}.#{current_year}")
```

Wynik:
`13.10.2021`

## Głębsze wgląd

Pobieranie bieżącej daty w Gleam opiera się na użyciu modułu `time`. Wewnątrz tego modułu znajduje się wiele funkcji, które pozwalają na łatwe operacje na danych dotyczących czasu i daty.

Na przykład, istnieje funkcja `time.from_timestamp(timestamp)` która pozwala na przekształcenie znacznika czasu (liczby sekund od 01-01-1970) na obiekt daty. Dzięki temu możemy pracować na większej liczbie danych dotyczących czasu.

Pamiętaj, że moduł `time` działa zgodnie z czasem uniwersalnego czasu koordynowanego (UTC), więc może być konieczne przekonwertowanie wyników na lokalną strefę czasową.

## Zobacz również

- Dokumentacja Gleam na temat pobierania bieżącej daty: https://gleam.run/documentation/stdlib/time.html#now
- Przykładowe projekty z użyciem modułu `time`: https://github.com/search?q=language%3Agleam+topic%3Atime+org%3Agleam-lang&type=Repositories
- Dyskusje społeczności na temat wykorzystania bieżącej daty w Gleam: https://github.com/gleam-lang/gleam/discussions?q=current+date