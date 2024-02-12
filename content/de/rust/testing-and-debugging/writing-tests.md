---
title:                "Tests Schreiben"
aliases: - /de/rust/writing-tests.md
date:                  2024-02-03T19:32:03.704943-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben von Tests in Rust umfasst das Erstellen automatisierter Überprüfungen, um sicherzustellen, dass Ihr Code wie erwartet funktioniert. Programmierer machen dies, um frühzeitig Fehler zu erkennen, Refactoring zu erleichtern und die Codequalität über die Zeit hinweg zu erhalten.

## Wie man:

Rusts integriertes Test-Framework unterstützt Einheits-, Integrations- und Dokumentationstests ohne die Notwendigkeit externer Bibliotheken. Tests werden mit `#[test]` annotiert, und jede so annotierte Funktion wird als Test kompiliert.

### Schreiben eines Einheitstests:

Platzieren Sie Einheitstests im Modul, das sie testen, indem Sie ein `tests` Untermodul verwenden, das mit `#[cfg(test)]` markiert ist, um sicherzustellen, dass sie nur beim Testen kompiliert werden.

```rust
// lib.rs oder main.rs
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

Tests ausführen:
```shell
$ cargo test
```

Ausgabe:
```shell
   Kompilieren Ihres Paketnamens v0.1.0 (/Pfad/zu/Ihrem/Paket)
    Fertiggestellt test [unoptimiert + debuginfo] Ziel(e) in 0,00 Sek.
     Laufende Unittests src/lib.rs (oder src/main.rs)

1 Test wird ausgeführt
test tests::it_adds_two ... ok

Testergebnis: ok. 1 bestanden; 0 fehlgeschlagen; 0 ignoriert; 0 gemessen; 0 ausgefiltert
```

### Schreiben von Integrationstests:

Integrationstests gehören in ein Tests-Verzeichnis auf der obersten Ebene Ihres Projekts, neben `src`. Jede `.rs` Datei in `tests` wird als eigene separate Crate kompiliert.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Testen mit beliebten Drittanbieter-Bibliotheken:

Für umfangreichere Testmöglichkeiten kann die Bibliothek `proptest` eine breite Palette von Eingaben erzeugen, um Funktionen zu testen.

Fügen Sie `proptest` als Entwicklungsabhängigkeit in `Cargo.toml` hinzu:

```toml
[dev-dependencies]
proptest = "1.0"
```

Verwenden Sie `proptest`, um den gleichen Test mit vielen automatisch generierten Eingaben auszuführen:

```rust
// innerhalb von tests/integration_test.rs oder eines #[cfg(test)] Moduls

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Dies überprüft, dass `add` bei einer breiten Palette von `i32` Eingaben nicht panisch wird.
