---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwersja daty na łańcuch (string) to proces zamiany formatu daty/czasu na sekwencję znaków, którą łatwo odczytać i zapisać. Programiści robią to, aby ułatwić manipulację i prezentację daty na różne sposoby.

## Jak to zrobić:

Użyjemy metody format!() do konwersji `SystemTime` na `String`. Podajemy specjalny format zamiany (%Y-%m-%d).

```Rust
use std::time::SystemTime;

fn main() {
    let system_time = SystemTime::now();
    let datetime: chrono::DateTime<chrono::Utc> = system_time.into();
    let date_string = datetime.format("%Y-%m-%d").to_string();
    println!("{}", date_string);
}
```
Wyjście to będzie łańcuch znaków reprezentujący dzisiejszą datę, np.: "2022-05-14"

## Dogłębne zrozumienie:

Metoda `format!()` pochodzi z języka C, gdzie używano funkcji `strftime()` do formatowania czasu. Metoda `format!()` w Rust jest bardziej bezpieczna typologicznie i wydajna.

Istnieją alternatywy dla metody `format!()`, takie jak `to_rfc3339()` i `to_rfc2822()`, które zwracają datę w specyfikacjach standardów internetowych. Wybór zależy od wymagań zastosowań.

As for implementation details, Rust's `format!()` relies on the `Display` trait, which dictates how types are formatted. This makes the code more robust and less prone to errors.

## Zobacz też:

Nie zapomnij odwiedzić oficjalnej dokumentacji Rust na temat [SystemTime](https://doc.rust-lang.org/std/time/struct.SystemTime.html) oraz dokumentacji biblioteki [Chrono](https://docs.rs/chrono/0.4.19/chrono/), jeśli chcesz dowiedzieć się więcej o konwersji dat.

Następnie można zapoznać się z również [Wgęszczenie kodu Rust'a](https://nnethercote.github.io/perf-book/size-bloat.html) i [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/), które zawierają wiele praktycznych przykładów i wskazówek dotyczących kodowania w Rust.