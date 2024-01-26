---
title:                "Pisanie testów"
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego? Pisanie testów to tworzenie kodu sprawdzającego, czy nasz program działa tak jak powinien. Programiści testują, by wyłapać błędy wcześnie, oszczędzić czas i zapewnić jakość.

## How to:
Stwórzmy proste testy w Rust. Załóżmy, że masz funkcję `dodaj(a: i32, b: i32) -> i32` do testowania:

```Rust
fn dodaj(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dodaj() {
        assert_eq!(dodaj(2, 2), 4);
    }

    #[test]
    #[should_panic]
    fn test_dodaj_fail() {
        assert_eq!(dodaj(2, 2), 5);
    }
}
```

Uruchom testy: `cargo test`

Wyniki powinny wyglądać tak:
```
running 2 tests
test tests::test_dodaj ... ok
test tests::test_dodaj_fail ... FAILED

failures:

failures:
    tests::test_dodaj_fail
```

## Deep Dive:
Historia: Testowanie w Rust zaczęło się od samego początku języka. Jest ono częścią kultury Rust – "Jeśli nie ma testów, nie jest to gotowe do produkcji." Alternatywy: Mozna użyć frameworków zewnętrznych jak `Proptest` czy `Quickcheck` dla testów właściwości. Implementacja: Rust używa atrybutów jak `#[test]` i `#[should_panic]` do określenia testów i oczekiwanych wyników.

## See Also:

1. [The Rust Programming Language - Testing](https://doc.rust-lang.org/book/ch11-00-testing.html)
2. [Rust by Example - Testing](https://doc.rust-lang.org/rust-by-example/testing.html)
3. [`Proptest` - Property testing with proptest](https://altsysrq.github.io/proptest-book/intro.html)
4. [`Quickcheck` crate documentation](https://docs.rs/quickcheck/latest/quickcheck/)
