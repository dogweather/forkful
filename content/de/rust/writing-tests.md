---
title:                "Rust: Tests schreiben"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil beim Schreiben von zuverlässigem und fehlerfreiem Code. Durch das Schreiben von Tests können mögliche Fehler frühzeitig erkannt und behoben werden, was zur Verbesserung der Codequalität und der Nutzererfahrung beiträgt. In diesem Blogbeitrag erfahren Sie, warum es wichtig ist, Tests zu schreiben.

## So geht's

Um Tests in Rust zu schreiben, können Sie die eingebaute Crate "test" verwenden. In den folgenden Beispielen werden wir die Funktion "add" testen, die zwei Zahlen addiert und das Ergebnis zurückgibt.

Zunächst müssen wir das "test" Crate in unser Projekt einbinden:

```Rust
extern crate test;
```

Dann erstellen wir eine Funktion, die wir später testen werden:

```Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}
```

Als nächstes müssen wir eine Testfunktion für unsere "add" Funktion erstellen. Diese Testfunktion muss mit dem Attribut "test" gekennzeichnet werden, damit der Rust Compiler sie als Test erkennt. In der Testfunktion können wir verschiedene Assertions verwenden, um zu überprüfen, ob das Ergebnis unserer Funktion korrekt ist.

```Rust
#[test]
fn test_add() {
    // Testfall 1: Überprüfen, ob 2 + 2 richtig addiert wird
    assert_eq!(add(2, 2), 4);

    // Testfall 2: Überprüfen, ob 5 + (-3) richtig addiert wird
    assert_eq!(add(5, -3), 2);
}
```

Um unsere Tests auszuführen, können wir das Kommando "cargo test" in der Konsole verwenden. Dies gibt uns eine Übersicht über alle bestandenen und fehlerhaften Tests.

## Tiefer gehen

Es gibt viele Möglichkeiten, Tests in Rust zu schreiben und zu organisieren. Sie können z.B. Tests in separate Moduldateien auslagern und diese dann mit dem Attribut "cfg(test)" markieren, um sie nur beim Testen zu kompilieren. Außerdem können Sie auch Property-based Testing verwenden, um zufällige Eingaben zu generieren und Ihre Funktionen damit zu testen.

Egal welche Art von Tests Sie schreiben, es ist wichtig, sich an die Prinzipien des Testens zu halten, wie z.B. den Single-Responsibility-Principle und die Verwendung von aussagekräftigen Testnamen.

## Siehe auch

- [Offizielle Rust Dokumentation zu Tests](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Einführung in das Schreiben von Tests in Rust](https://opensource.com/article/19/6/getting-started-testing-rust)
- [Property-based Testing mit Rust](https://www.fpcomplete.com/blog/quickcheck-in-rust/)