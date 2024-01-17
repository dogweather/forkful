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

## Was & Warum?

Das Verfassen von Tests ist ein wichtiger Bestandteil der Softwareentwicklung. Es beinhaltet das Schreiben von Code, der bestimmte Funktionen oder Teile eines Programms überprüft, um sicherzustellen, dass sie wie erwartet funktionieren. Programmer schreiben Tests, um die Qualität und Zuverlässigkeit ihres Codes zu verbessern und mögliche Fehler zu identifizieren, bevor sie in die Produktionsumgebung gelangen.

## Wie geht's?

Das Schreiben von Tests in Rust ist einfach und effektiv. Hier ist ein einfaches Beispiel:

```Rust
// Definiere eine Funktion, die zwei Zahlen addiert
fn sum(x: i32, y: i32) -> i32 {
    return x + y;
}

// Schreibe einen Test zur Überprüfung ob die Funktion korrekt funktioniert
#[test]
fn test_sum() {
    assert_eq!(sum(2, 3), 5); // Erwartetes Ergebnis: 5
}
```

Der ```#[test]``` -Attribut sagt dem Rust-Compiler, dass dies ein Test ist, der bei jedem Durchlauf des Codes ausgeführt werden sollte. In diesem Beispiel verwenden wir die Funktion ```assert_eq!```, die das erwartete Ergebnis mit dem tatsächlichen Ergebnis vergleicht und einen Fehler ausgibt, wenn sie nicht übereinstimmen.

Hier ist die Ausgabe, die wir erhalten, wenn wir den Test ausführen:

```bash
running 1 test
test test_sum ... ok

test result: ok. 1 passed, 0 failed, 0 ignored, 0 measured, 0 filtered out
```

Rust bietet auch weitere nützliche Makros für das Testen, wie z.B. ```assert_ne!``` zum Überprüfen, ob zwei Werte ungleich sind.

## Tiefer Einblick

Das Testen von Code ist ein wichtiger Bestandteil der Softwareentwicklung, der schon seit vielen Jahren praktiziert wird. In der Vergangenheit wurden häufig manuelle Tests durchgeführt, bei denen ein Programm von Hand ausgeführt und beobachtet wurde, ob es wie erwartet funktioniert. Dies war jedoch sehr zeitaufwändig und ungenau. Mit der Einführung von automatisierten Tests, wie z.B. in Rust, können Programmierer schnell und effizient sicherstellen, dass ihr Code richtig funktioniert.

Es gibt auch andere Möglichkeiten, Code zu testen, wie z.B. durch die Verwendung von Test-Frameworks oder Continuous Integration. Diese Ansätze bieten zusätzliche Funktionen und Flexibilität, sind aber oft auch komplexer zu implementieren und erfordern mehr Aufwand in der Wartung.

In Rust können Tests auch parallel ausgeführt werden, wodurch die Geschwindigkeit und Effizienz verbessert wird. Dies ist aufgrund der starken Typisierung und strikten Verwendung von Threads in Rust möglich.

## Siehe auch

- [The Rust Book](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
- [Testing in Rust](https://medium.com/@nshntarora/testing-in-rust-d282b5aa9742)