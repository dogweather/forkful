---
title:    "Rust: Duży ciąg znaków"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów, może zaistnieć potrzeba zmiany wielkości liter w ciągu znaków. Na przykład, gdy chcemy wyświetlić nazwisko użytkownika zawsze z dużej litery, niezależnie od tego jak zostało wprowadzone. W takiej sytuacji, używamy funkcji "capitalize" w języku Rust.

## Jak to zrobić

Możemy użyć metody "capitalize" na obiekcie typu String, aby zmienić pierwszą literę w ciągu na dużą. Przykładowy kod poniżej pokazuje jak to zrobić:

``` Rust
let name = String::from("jan kowalski");
let capitalized_name = name.capitalize();
println!("Imię użytkownika: {}", capitalized_name);

// Wynik: Imię użytkownika: Jan kowalski
```

W powyższym przykładzie, wywołujemy funkcję "capitalize" na obiekcie "name" typu String. Następnie, przy użyciu metody "println!", wyświetlamy zmienione imię użytkownika.

### Uwaga

Warto zauważyć, że funkcja "capitalize" zmienia tylko pierwszą literę w ciągu, pozostałe litery nie są zmieniane.

## Głębszy wgląd

Funkcja "capitalize" jest zaimplementowana w bibliotece standardowej języka Rust, dla typu String. Jej dokładna implementacja może wyglądać tak:

```Rust
pub fn capitalize(&self) -> String {
    let mut chars = self.chars();
    match chars.next() {
        None => String::new(),
        Some(first_char) => first_char.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
```

Ta implementacja pokazuje jak funkcja ta jest w stanie zmienić pierwszą literę w ciągu na dużą przy użyciu metody "collect", która łączy znaki jako nowy obiekt typu String.

## Zobacz także

- [Dokumentacja języka Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.capitalize)
- [Tutorial: Rust w 30 minut](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html)
- [Wprowadzenie do programowania w Rust](https://www.rust-lang.org/learn/get-started)