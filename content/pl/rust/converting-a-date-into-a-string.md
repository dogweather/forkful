---
title:                "Rust: Konwertowanie daty na ciąg znaków"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na łańcuch znaków jest powszechnym wyzwaniem w programowaniu. Czasami musimy wyświetlić datę w czytelny sposób dla użytkownika lub zapisać ją w odpowiednim formacie do pliku. W tym poście dowiesz się, jak wykonać tę konwersję w języku Rust.

## Jak to zrobić

Istnieją różne metody konwersji daty na łańcuch znaków w języku Rust. Jedną z najpopularniejszych jest użycie biblioteki "chrono". Spójrzmy na przykładowy kod:

```Rust
use chrono::{DateTime, Utc, Datelike, Timelike};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    let date_string = now.format("%Y-%m-%d").to_string();
    println!("{}", date_string);
}
```

W tym przykładzie, używamy funkcji `format()` do sformatowania daty według podanego szablonu, a następnie używamy funkcji `to_string()` aby konwertować ją na łańcuch znaków. Wynik powyższego kodu będzie wyglądał następująco: `2021-08-27`.

Możemy także wybrać inny format, na przykład `"%d/%m/%Y"`, który wyświetli datę w postaci: `27/08/2021`. Pełna lista dostępnych szablonów znajduje się w dokumentacji biblioteki "chrono".

## Deep Dive

Podczas konwersji daty na łańcuch znaków, ważne jest aby pamiętać o strefie czasowej. W powyższym przykładzie użyliśmy strefy czasowej "Utc". Jeśli jednak potrzebujemy wyświetlić datę w odpowiednim dla nas regionie czasowym, musimy uwzględnić to przy konwersji. Możemy to zrobić poprzez użycie funkcji `with_timezone()` i przekazanie jej odpowiedniej strefy czasowej.

## Zobacz też

- [Dokumentacja biblioteki "chrono"](https://docs.rs/chrono/0.4.19/chrono/)
- [Wideo o konwertowaniu daty na łańcuch znaków w języku Rust](https://www.youtube.com/watch?v=k0Akn3rnFck)
- [Artykuł o konwersji daty na łańcuch znaków w języku Rust](https://dev.to/ybanis/konwersja-daty-na-lancuch-znakow-w-jezyku-rust-4f8a)