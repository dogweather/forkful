---
title:                "Skriva tester"
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester innebär att kodas scener för att automatiskt kontrollera att programmet beter sig som förväntat. Programmerare gör detta för att snabbt upptäcka buggar, säkerställa kodkvalitet och förenkla underhållet.

## Hur man gör:

För att skapa ett test i Rust, lägg till en `#[test]` annotering ovanför en funktion. Kör testerna med `cargo test`.

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn det_funkar() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    #[should_panic]
    fn det_failar() {
        assert!(false);
    }
}

```

Kör dina tester:

```bash
$ cargo test
```

Förväntad output:

```
running 2 tests
test tests::det_failar ... FAILED
test tests::det_funkar ... ok

failures:

failures:
    tests::det_failar

test result: FAILED. 1 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out
```

## Djupdykning

Testning i Rust började prioriteras under 2015. Alternativ till Rusts inbyggda ramverk inkluderar integrationstester, dokumentationstester och externa verktyg som `quickcheck`. Implementeringsdetaljer: Använd `cfg(test)` för att indikera att koden bara ska kompileras vid testning och attribut som `#[should_panic]` för att ange förväntade panikfall.

## Se även:

- Rusts officiella bok om testning: https://doc.rust-lang.org/book/ch11-00-testing.html
- Rust `assert` makron: https://doc.rust-lang.org/std/macro.assert.html
- `quickcheck` biblioteket: https://docs.rs/quickcheck/*/quickcheck/
