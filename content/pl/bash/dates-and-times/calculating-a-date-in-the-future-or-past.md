---
date: 2024-01-20 17:28:36.397173-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.598629-06:00'
model: gpt-4-1106-preview
summary: .
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

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
