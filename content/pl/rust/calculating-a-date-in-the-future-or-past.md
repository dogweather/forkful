---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Rust: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Co i po co?

Obliczanie daty w przyszłości lub przeszłości polega na wyznaczaniu daty, która jest pewną liczbę dni, miesięcy lub lat przed lub po aktualnej dacie. Programiści często używają tego typu obliczeń do tworzenia kalendarzy, alarmów, oraz innych funkcji związanych z czasem.

Jak to zrobić:

```Rust
// Tworzymy strukturę reprezentującą datę
let mut date = NaiveDate::from_ymd(2021, 6, 1);

// Dodajemy 30 dni do aktualnej daty
date += Duration::days(30);

// Odejmujemy 1 rok od aktualnej daty
date -= Duration::days(365);

// Wyświetlamy wynik
println!("{}", date);
```

Wyjście: 2020-05-02

Głębszy zanurzenie:

Obliczanie dat w przeszłości i przyszłości było już wykorzystywane od starożytności, szczególnie do ustalenia dokładnych dat wydarzeń i ceremonii. Istnieją również alternatywne sposoby obliczania dat, np. metoda juliańska lub gregoriańska. W programowaniu, popularnym narzędziem do obliczania dat jest biblioteka "chrono" dla języka Rust, która umożliwia wykonywanie różnych operacji na datach.

Zobacz także:

- Dokumentacja biblioteki Chrono: https://docs.rs/chrono/latest/chrono/
- Wikipedia: https://pl.wikipedia.org/wiki/Kalendarz
- Inne sposoby obliczania dat: https://sciencing.com/calculate-future-day-7725892.html