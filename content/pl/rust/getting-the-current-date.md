---
title:                "Uzyskiwanie bieżącej daty"
html_title:           "Rust: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie bieżącej daty jest częstym zadaniem w programowaniu, szczególnie w przypadku aplikacji, które muszą wyświetlać aktualny czas lub datę wykonania określonej operacji. Dzięki językowi Rust możemy szybko i łatwo dostać obecną datę w naszych programach.

## Jak to zrobić

Język Rust oferuje kilka sposobów na pobieranie bieżącej daty. Najprostszym i najczęściej wykorzystywanym jest użycie standardowej biblioteki chrono, która dostarcza funkcji i typów do obsługi dat i czasów.

W pierwszym kroku musimy zaimportować bibliotekę chrono, co można zrobić przy użyciu polecenia `use chrono::prelude::*;`. Następnie możemy użyć funkcji `Local::now()` aby uzyskać obecny czas i datę w lokalnej strefie czasowej. Poniższy kod przedstawia przykład pobierania bieżącej daty i wyświetlenia jej w konsoli:

```Rust
use chrono::prelude::*;

fn main() {
    let current_date = Local::now().format("%Y-%m-%d").to_string();
    println!("Bieżąca data: {}", current_date);
}
```

Output:

```
Bieżąca data: 2021-10-17
```

Możemy także uzyskać bieżący czas i datę w innych strefach czasowych, używając odpowiednich funkcji, takich jak `Utc::now()` dla Uniwersalnego Czasu Koordynowanego (UTC) lub `FixedOffset::east(8).now()` dla strefy czasowej GMT+8. Wszystkie te funkcje zwracają obiekt typu `DateTime` z dokładnym czasem i datą, które możemy sformatować według naszych potrzeb.

## Głębszy wgląd

Biblioteka chrono dostarcza wiele innych funkcji, takich jak obliczanie różnicy czasu między dwoma datami, konwersja między strefami czasowymi czy obliczanie dnia tygodnia dla danej daty. Dodatkowo, Rust udostępnia także moduł `std::time` z funkcjami, takimi jak `SystemTime::now()`, które pozwalają na pobranie obecnego czasu w postaci liczby sekund od startu komputera.

Pamiętaj, że pobieranie daty i czasu w programowaniu jest zawsze zależne od strefy czasowej, w której się znajdujemy, dlatego ważne jest, aby dokładnie określić strefę czasową w naszych aplikacjach.

## Zobacz także

- [Dokumentacja biblioteki chrono](https://docs.rs/chrono)
- [Poradnik o datach i czasie w języku Rust](https://learnrustwith.github.io/book/keywords/date_time.html)
- [Dokumentacja modułu std::time](https://doc.rust-lang.org/std/time/)