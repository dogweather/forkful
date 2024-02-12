---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases:
- /pl/rust/capitalizing-a-string/
date:                  2024-02-03T19:06:38.122284-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Zmiana pierwszej litery ciągu znaków na wielką w języku Rust polega na modyfikacji ciągu tak, aby jego pierwszy znak był dużą literą, jeśli jest literą, pozostawiając resztę ciągu bez zmian. Programiści często wykonują tę operację w celach formatowania, takich jak przygotowanie słów do tytułów czy zapewnienie spójności w danych wprowadzanych przez użytkownika.

## Jak to zrobić:

Aby zamienić pierwszą literę ciągu na wielką w Rust, masz dwie główne drogi: używanie funkcji biblioteki standardowej lub korzystanie z zewnętrznych crate'ów dla bardziej złożonych lub specyficznych potrzeb. Oto jak możesz to zrobić oboma sposobami.

### Korzystanie z biblioteki standardowej Rust

Biblioteka standardowa Rust nie oferuje bezpośredniej metody na zamianę ciągów na ciągi z pierwszą wielką literą, ale możesz tego dokonać, manipulując znakami ciągu.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // Wyjście: Hello
}
```

### Korzystając z crate'a `heck`

Dla bardziej bezpośredniego podejścia, szczególnie gdy pracujesz w szerszym kontekście przetwarzania tekstu, możesz preferować korzystanie z zewnętrznych bibliotek, takich jak `heck`. Crate `heck` oferuje różne funkcjonalności konwersji przypadków, w tym prosty sposób na zmianę ciągów na ciągi z pierwszą wielką literą.

Najpierw dodaj `heck` do swojego `Cargo.toml`:

```toml
[dependencies]
heck = "0.4.0"
```

Następnie użyj go, żeby zamienić pierwszą literę na wielką:

```rust
extern crate heck; // Nie jest potrzebne w edycji Rust 2018 lub późniejszej
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Wyjście: Hello World
}
```

Uwaga: Metoda `to_title_case` zapewniona przez `heck` zmienia pierwsze litery wszystkich słów w ciągu na wielkie, co może być więcej niż oczekujesz, jeśli chcesz zmienić tylko pierwszy znak ciągu. Dostosuj jej użycie zgodnie ze swoimi specyficznymi potrzebami.
