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

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu dni, miesięcy czy lat od konkretnej daty. Programiści robią to, by przewidzieć okoliczności, np. koniec okresu próbnego, data ważności etc.

## Jak to zrobić:

W Rust, możemy to zrealizować za pomocą pakietu `chrono`. Oto jak wygląda w praktyce:

```Rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    println!("Teraz: {}", now);

    let future: DateTime<Utc> = now + Duration::days(5);
    println!("Za 5 dni: {}", future);

    let past: DateTime<Utc> = now - Duration::days(5);
    println!("5 dni temu: {}", past);
}
```

Przykładowe wyjście:

```bash
Teraz: 2022-08-26 11:55:06.736320 UTC
Za 5 dni: 2022-08-31 11:55:06.736320 UTC
5 dni temu: 2022-08-21 11:55:06.736320 UTC
```

## Głębszy Wgląd

Obliczanie daty w przyszłości lub przeszłości jest tak starą praktyką jak sama nauka programowania. Historycznie, programiści musieli samodzielnie implementować te operacje, uwzględniając wszystkie niuanse związane z latami przestępnymi, różnymi kalendarzami, strefami czasowymi itp.

Jako alternatywę, można skorzystać z wielu bibliotek języka Rust takich jak 'time' czy 'date'. Można również skorzystać z bibliotek systemowych.

Niektóre szczegóły implementacji do rozważenia podczas pracy z datami i czasem: Rust używa czasu systemowego, który jest podatny na zmiany (na przykład, kiedy zmieniamy czas na urządzeniu lub synchronizujemy czas z serwerem). Może to wpłynąć na obliczenia zależne od upływu czasu.

## Zobacz też:

- [Chrono Documentation](https://docs.rs/chrono/): Dokumentacja paczki `chrono`, której używamy w powyższym przykładzie.
- [Rust Programming](https://www.rust-lang.org/learn): Zarówno dla początkujących jak i zaawansowanych, strona zawiera przydatne informacje o programowaniu w Rust.