---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben heißt, Code zur Überprüfung deines Codes zu erstellen, um sicherzustellen, dass er wie erwartet funktioniert. Programmierer machen das, um Bugs zu vermeiden, die Codequalität zu erhöhen und das Vertrauen in ihren Code zu stärken.

## Anleitung:
```Rust
// Im lib.rs oder main.rs File

#[cfg(test)]
mod tests {
    #[test]
    fn es_testet_etwas() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    #[should_panic(expected = "Das hätte nicht passieren dürfen!")]
    fn es_sollte_panic() {
        panic!("Das hätte nicht passieren dürfen!");
    }
}

// Führe Tests aus mit:
// $ cargo test

// Mögliche Ausgabe:
// running 2 tests
// test tests::es_testet_etwas ... ok
// test tests::es_sollte_panic ... ok

// test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Vertiefung:
Tests in Rust haben seit Rust 1.0 hohen Stellenwert. `cargo test` ist ein starkes Werkzeug, das Tests einfach macht. Alternativen zu den eingebauten Test-Frameworks sind `proptest` oder `quickcheck`, wenn es um Property-based Testing geht. Details: Tests können parallel laufen, um Zeit zu sparen und Tests sind oft in Dokumentation integriert, genannt Doc-Tests.

## Siehe auch:
- The Rust Programming Language (Buch): https://doc.rust-lang.org/book/ch11-00-testing.html
- Rust by Example – Testing: https://doc.rust-lang.org/rust-by-example/testing.html
- Rust Documentation zu Testing: https://doc.rust-lang.org/cargo/guide/tests.html