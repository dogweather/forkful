---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pobieranie aktualnej daty w Rust: Szybki przewodnik dla programistów

## Co i dlaczego?
Pobieranie aktualnej daty to jedna z najczęstszych operacji w naszej pracy. Czy to do śledzenia zdarzeń, generowania raportów, czy ustalania terminów, ta prosta czynność ma wiele zastosowań.

## Jak to zrobić:

Współczesna wersja Rust, umożliwia pobranie aktualnej daty w dosyć prosty sposób za pomocą wbudowanej funkcji `SystemTime::now`. Oto proste użycie:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
fn main(){
    let now = SystemTime::now();
    let since_the_epoch = now.duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    println!("{:?}", since_the_epoch);
}
```
Po uruchomieniu powyższego kodu, zwróci on liczbę sekund od epoki Unix (1 stycznia 1970).

## Głebokie zanurzenie:

**Kontekst historyczny:** W Rust pierwotnie nie było łatwego sposobu na pobieranie daty i czasu. Więc to jest swego rodzaju umiejętność, która wymagała wielu dodatkowych kroków i manipulacji, aby móc ją uzyskać..

**Alternatywy:** Do pobierania daty i czasu w Rust można również używać zewnętrznych bibliotek jak Chrono. Chrono dostarcza pełny zestaw narzędzi do manipulacji datą i czasem w wygodny sposób.

**Szczegóły implementacji:** Rust opiera swoje zarządzanie czasem na epoce Unix, czyli na liczbie sekund, które upłynęły od 1 stycznia 1970 roku. W związku z tym musimy zawsze przekształcić czas systemowy na odpowiedni format, który chcemy uzyskać.

## Zobacz także:

1. Dokumentacja Rust na temat zarządzania czasem: https://doc.rust-lang.org/std/time/
2. Dokumentacja Chrono: https://docs.rs/chrono/0.4.19/chrono/
3. Przewodnik Rust do zarządzania czasem: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html#misconception-3---the-cure-to-all-your-problems-adding--static