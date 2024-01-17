---
title:                "Uzyskiwanie aktualnej daty"
html_title:           "Rust: Uzyskiwanie aktualnej daty"
simple_title:         "Uzyskiwanie aktualnej daty"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zdobycie aktualnej daty to proces, w którym programista pobiera informację o czasie i dacie w chwili uruchomienia programu. Jest to przydatne dla programów, które muszą operować na danych w zależności od aktualnego dnia lub wykonywać operacje związane z kalendarzem.

## Jak to zrobić:
Mamy kilka sposobów na pobranie aktualnej daty w języku Rust. Zobaczmy przykłady poniżej.

```Rust
extern crate chrono; // importujemy bibliotekę

use chrono::{Utc, Local}; // określamy strefy czasowe

fn main() {
   let utc = Utc::now(); // pobieramy datę i czas aktualnej strefy UTC
   let local = Local::now(); // pobieramy datę i czas lokalnej strefy czasowej

   // wydrukowanie do konsoli
   println!("{}", utc); // 2020-06-25 12:00:00 UTC
   println!("{}", local); // 2020-06-25 14:00:00 CEST
}
```

## Głębszy wywód:
Pobieranie aktualnej daty jest procesem, który ma długą historię w świecie programowania. Dawniej, w językach takich jak C czy C++, programiści musieli ręcznie obsługiwać funkcje związane z datami. W języku Rust jest to zdecydowanie łatwiejsze dzięki bibliotece chrono, która zapewnia mnóstwo pomocnych metod i funkcji. Istnieją również inne alternatywy takie jak biblioteka time, ale chrono jest najczęściej wybieraną opcją ze względu na swoją funkcjonalność.

## Zobacz też:
- Oficjalna dokumentacja biblioteki chrono: https://docs.rs/chrono/
- Przykładowy kod pobierania aktualnej daty w języku Rust: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=22eabd7dbfc1a57374db39a0726f1ca3.