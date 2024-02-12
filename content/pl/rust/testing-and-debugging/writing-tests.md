---
title:                "Pisanie testów"
aliases:
- /pl/rust/writing-tests/
date:                  2024-02-03T19:32:14.151331-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w Rust polega na tworzeniu automatycznych kontroli, aby zapewnić, że kod działa zgodnie z oczekiwaniami. Programiści robią to, aby wcześnie wykrywać błędy, ułatwić refaktoryzację i utrzymać jakość kodu na przestrzeni czasu.

## Jak to zrobić:

Wbudowany framework testowy Rusta wspiera testy jednostkowe, integracyjne oraz dokumentacyjne bez potrzeby korzystania z zewnętrznych bibliotek. Testy są oznaczane za pomocą `#[test]`, a każda funkcja tak oznaczona jest kompilowana jako test.

### Pisząc test jednostkowy:

Umieść testy jednostkowe w module, który testują, używając podmodułu `tests`, oznaczonego za pomocą `#[cfg(test)]`, aby zapewnić, że będą kompilowane tylko podczas testowania.

```rust
// lib.rs lub main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Uruchamianie testów:
```shell
$ cargo test
```

Wynik:
```shell
   Compiling your_package_name v0.1.0 (/ścieżka/do/twojego_pakietu)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (lub src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Pisząc testy integracyjne:

Testy integracyjne umieszcza się w katalogu tests na najwyższym poziomie projektu, obok `src`. Każdy plik `.rs` w `tests` jest kompilowany jako oddzielny crate.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Testowanie z popularnymi bibliotekami innych firm:

Dla bardziej zaawansowanych możliwości testowania, biblioteka `proptest` może generować szeroki zakres danych wejściowych do testowania funkcji.

Dodaj `proptest` jako zależność deweloperską w `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Użyj `proptest` do uruchomienia tego samego testu z wieloma automatycznie wygenerowanymi danymi wejściowymi:

```rust
// wewnątrz tests/integration_test.rs lub modułu #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

To sprawdza, czy `add` nie powoduje paniki dla szerokiego zakresu danych wejściowych `i32`.
