---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:28:36.397173-07:00
model:                 gpt-4-1106-preview
html_title:           "Clojure: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to ustalanie dokładnej daty przed lub po określonym czasie. Programiści robią to, by obsługiwać zadania związane z terminami, harmonogramami, czy wygaśnięciem ważności danych.

## Jak to zrobić:
```Bash
# Obliczanie daty 10 dni w przyszłości
date -d "+10 days"

# Obliczanie daty 5 tygodni w przeszłości
date -d "-5 weeks"
```

Przykładowe wyjście może wyglądać tak:
```Bash
$ date -d "+10 days"
śro, 12 kwi 2023, 17:41:22 CEST

$ date -d "-5 weeks"
wto, 28 lut 2023, 17:41:27 CET
```

## W głębi tematu:
Data i czas to fundamenty wielu aplikacji. Od kalendarzy po systemy rezerwacyjne, potrzeba mierzenia czasu jest wszechobecna. W historii, z datami walczyliśmy różnymi narzędziami: od `cron` do skomplikowanych systemów baz danych.

Alternatywy dla bashowej `date` obejmują:

- `dateutils` - kolekcja narzędzi do szybkiej manipulacji datami,
- Skrypty w Pythonie używające modułu `datetime`,
- `at` i `cron` dla zaplanowanych zadań.

Szczegóły implementacji zawierają formatowanie daty według potrzeb przy użyciu + i - dla określenia przyszłości i przeszłości oraz formatów dat, np. `%Y-%m-%d`.

## Zobacz również:
- Strony `man` dla poleceń `date` i `at`.
- Dokumentacja GNU Coreutils: https://www.gnu.org/software/coreutils/manual/coreutils.html
- Projekt Dateutils: http://www.fresse.org/dateutils/
- Python `datetime` moduł: https://docs.python.org/3/library/datetime.html
