---
title:    "Rust: Konwertowanie daty na ciąg znaków"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest często potrzebna w programowaniu, aby wyświetlić datę lub przetworzyć ją w innym formacie. W tym poście pokażemy Ci, jak łatwo wykonać tę operację w języku Rust.

## Jak to zrobić

```Rust
use chrono::{DateTime, Utc,format::strftime};
 
fn main() {
    let now: DateTime<Utc> = Utc::now();
    // Konwersja daty na format ISO 8601
    let iso_date_string = now.to_rfc3339();
    println!("Data w formacie ISO: {}", iso_date_string);
    // Konwersja daty na format DD-MM-YYYY
    let custom_date_string = now.format("%d-%m-%Y").to_string();
    println!("Data w formacie DD-MM-YYYY: {}", custom_date_string);
    // Konwersja daty na własny format
    let my_date_format = "%A, %d %B %Y, godzina %R";
    let my_date_string = now.format(my_date_format).to_string();
    println!("Moja własna data: {}", my_date_string);
}
```

Output:
```
Data w formacie ISO: 2020-08-31T15:28:42.873962981+00:00
Data w formacie DD-MM-YYYY: 31-08-2020
Moja własna data: poniedziałek, 31 sierpnia 2020, godzina 15:28
```

W powyższym kodzie wykorzystujemy bibliotekę `chrono`, która jest bardzo popularna w Rust do operacji na datach i godzinach. Najpierw importujemy niezbędne funkcje, a następnie tworzymy obiekt daty `now`, z którego będziemy korzystać. Następnie, korzystając z metody `to_rfc3339()` konwertujemy datę na format ISO 8601. W kolejnych liniach wykorzystujemy metodę `format()` do ustawienia własnego formatu wyjściowego. Możesz dowolnie modyfikować format, aby uzyskać pożądany wynik. Na końcu wyświetlamy wynik przy użyciu funkcji `to_string()`.

## Deep Dive

Rust oferuje wiele opcji konwersji daty na ciąg znaków, ponieważ jest to często wymagana operacja w programowaniu. Możesz również zmieniać strefę czasową lub przetwarzać daty i godziny w różnych strefach przy użyciu pojęcia `TimeZone`. Możesz również wykorzystać funkcję `strftime()` do zmiany formatu daty na inny niż standardowe dostępne w języku Rust.

Ważne jest, aby pamiętać, że konwersja daty na ciąg znaków może być czasochłonna, dlatego ważne jest, aby wybierać optymalne rozwiązania i nie wykonywać konwersji bez potrzeby. W przypadku przetwarzania dużej ilości danych lub powtarzając tego typu operacje, należy zwrócić uwagę na wydajność kodu.

## Zobacz także

- Dokumentacja biblioteki `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- Blog post na temat użycia biblioteki `chrono` do operacji na datach i godzinach: https://blog.logrocket.com/working-with-dates-and-times-in-rust-using-chrono/
- Kurs na temat języka Rust: https://www.rust-lang.org/learn