---
title:    "Gleam: Sprawdzanie czy istnieje katalog"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami musimy sprawdzić, czy dany katalog istnieje, aby upewnić się, czy możemy w nim zapisać lub odczytać pliki. W tym blogu dowiesz się, jak sprawdzić istnienie katalogu w języku programowania Gleam.

## Jak to zrobić

Sprawdzenie, czy katalog istnieje w języku Gleam jest bardzo proste dzięki funkcji `os.dir_exists`. Wystarczy podać ścieżkę do katalogu jako argument funkcji, a wynik będzie zawierał wartość `true` lub `false`.

```Gleam
let exist = os.dir_exists("sciezka/do/katalogu")
```

Jeśli katalog istnieje, wartość `exist` będzie równa `true`, w przeciwnym wypadku będzie równa `false`.

## Głębsza analiza

Podczas sprawdzania, czy katalog istnieje, może pojawić się kilka problemów, na które warto zwrócić uwagę. Jednym z nich jest możliwość błędnego zapisania ścieżki, co spowoduje niepoprawne działanie funkcji. Ważne jest również upewnienie się, że funkcja `os.dir_exists` jest wywoływana z prawidłowymi argumentami.

## Zobacz również

- Dokumentacja funkcji `os.dir_exists` w języku Gleam: https://gleam.run/function/os.dir_exists.html
- Tutoriale i przykłady programów w języku Gleam: https://gleam.run/docs.html
- Kurs programowania w języku Gleam w języku polskim: https://gleam.run/szkolenie.html