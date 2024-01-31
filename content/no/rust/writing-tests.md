---
title:                "Skriving av tester"
date:                  2024-01-19
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Testing er å sjekke at koden din virker som den skal. Tester oppdager bugs og sørger for at tingen du bygger faktisk fungerer.

## Hvordan:
```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn det_virker() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    #[should_panic]
    fn det_feiler() {
        assert!(false);
    }
}

fn main() {
    println!("Hei på testing!");
}
```
Kjøre tester:
```bash
$ cargo test
```
Forventet resultat:
```
running 2 tests
test tests::det_feiler ... FAILED
test tests::det_virker ... ok

failures:

---- tests::det_feiler stdout ----
thread 'main' panicked at 'assertion failed', src/main.rs:10:9
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace

failures:
    tests::det_feiler

test result: FAILED. 1 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out
```

## Dybdeinformasjon:
Testing i Rust har utviklet seg fra enkle `assert` makroer til et fullverdig testrammeverk integrert i Cargo. Alternativer inkluderer integrasjonstester, doc-tester og eksterne rammeverk som `quickcheck`. Tester kjøres parallelt for effektivitet, men det kan endres med `--test-threads`.

## Se Også:
- Rust Book Testing: https://doc.rust-lang.org/book/ch11-00-testing.html
- Cargo Test Documentation: https://doc.rust-lang.org/cargo/commands/cargo-test.html
- Rust by Example - Testing: https://doc.rust-lang.org/rust-by-example/testing.html
