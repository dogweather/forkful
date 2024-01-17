---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Rust: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Konwertowanie daty na ciąg znaków jest zwykłym zadaniem, które polega na zamianie informacji o dacie w formacie przechowywanym przez komputer na czytelną dla ludzi postać. Programiści często wykonują tę czynność, aby umożliwić użytkownikom wygodne korzystanie z aplikacji, bądź aby wyświetlać datę w różnych formatach.

## Jak to zrobić:

```Rust
use chrono::prelude::*;

fn main() {
    let date = Utc::now();
    let date_string = date.format("%Y-%m-%d").to_string();
    println!("{}", date_string);
}
```

W powyższym przykładzie użyto biblioteki `chrono`, która jest popularnym narzędziem w Rust do operacji na datach i czasie. Funkcja `format` pozwala nam ustalić pożądany format dla daty, a następnie używamy metody `to_string` do zamiany daty na ciąg znaków. Ostatecznie wyświetlamy wynik za pomocą funkcji `println!`.

## Dogłębnie:

### Kontekst historyczny:

Zadanie konwertowania daty na ciąg znaków jest powszechne w programowaniu i stało się jeszcze bardziej popularne dzięki rozwojowi aplikacji internetowych i mobilnych. W przeszłości, aby wyświetlić datę w różnych formatach, programiści musieli ręcznie manipulować danymi, co było czasochłonne i podatne na błędy.

### Alternatywy:

Niektóre języki programowania posiadają wbudowane narzędzia do konwertowania daty na ciąg znaków, na przykład funkcję `strftime` w języku C. W Rust jednak, korzystanie z biblioteki takiej jak `chrono` jest powszechne i wygodne.

### Szczegóły implementacji:

Konwersja daty na ciąg znaków w Rust jest wykonywana przez bibliotekę lub samodzielnie przez użytkownika przy użyciu funkcji `format`. Powstały ciąg znaków jest następnie zwracany lub wyświetlany na ekranie. Kluczowe jest tu użycie odpowiedniego formatu, aby otrzymać pożądany rezultat.

## Zobacz również:

- Dokumentacja biblioteki `chrono` dla Rust: https://docs.rs/chrono/
- Przewodnik po konwersji daty i formatowaniu w Rust: https://stevedonovan.github.io/rust-gentle-intro/6-dates.html