---
title:                "Tests schreiben"
html_title:           "Rust: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt die Mühe machen, Tests zu schreiben? Es gibt mehrere Gründe dafür:

- Tests helfen dabei, Bugs frühzeitig zu erkennen und zu beheben, was langfristig viel Zeit und Nerven spart.
- Durch Tests kann man sich sicherer sein, dass der Code das tut, was er soll, und dass Änderungen keine ungewollten Nebenwirkungen haben.

## Wie es geht
Um Tests in Rust zu schreiben, benötigst du das Modul `test` aus dem `std`-Paket. Hier findest du ein einfaches Beispiel:

```Rust
// Modul `test` importieren
use std::test;

// Funktion, die getestet werden soll
fn multiplizieren(x: i32, y: i32) -> i32 {
    x * y        
}

// Tests mit `#[test]`-Attribut markieren
#[test]
fn test_multiplizieren() {
    assert_eq!(multiplizieren(2, 3), 6); // erwartetes Ergebnis angeben
    assert_eq!(multiplizieren(5, -2), -10);
}
```

Die `assert_eq!`-Makros vergleichen das Ergebnis der Funktion mit dem erwarteten Wert und geben eine Fehlermeldung aus, wenn sie nicht übereinstimmen. Der Output sollte in etwa so aussehen:

```
running 1 test
test test_multiplizieren ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Neben `assert_eq!` gibt es noch weitere nützliche Makros, wie zum Beispiel `assert_ne!`, welches auf Ungleichheit überprüft.

## Tiefer tauchen
Wenn du mehr über das Schreiben von Tests in Rust erfahren möchtest, empfehle ich dir folgende Ressourcen:

- Offizielle Dokumentation zu [Unit-Tests](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html) und [Tests für Fehlerbehandlung](https://doc.rust-lang.org/rust-by-example/testing/panic.html) in Rust.
- [Rust Testing Cheat Sheet](https://devhints.io/rust-testing) für eine Übersicht über alle verfügbaren Test-Makros.
- [The Rust Book](https://doc.rust-lang.org/book/) für eine umfassende Einführung in die Programmiersprache Rust und ihre Features, einschließlich Tests.

## Siehe auch
- [The Rust Programming Language](https://www.rust-lang.org/) für offizielle Informationen und Neuigkeiten zu Rust.
- [Awesome Rust](https://github.com/rust-unofficial/awesome-rust) für eine Liste mit hilfreichen Tools, Libraries und Ressourcen rund um Rust.