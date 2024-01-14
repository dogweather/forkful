---
title:    "Rust: Tests schreiben"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Softwareentwicklung ist das Schreiben von Tests oft ein wesentlicher Bestandteil des Prozesses. Gleichzeitig ist es jedoch auch eine lästige und zeitraubende Aufgabe, die oft vernachlässigt wird. Warum sollte man sich also die Mühe machen, Tests zu schreiben?

Tests sind eine essentielle Maßnahme, um die Qualität unserer Software zu gewährleisten. Sie helfen uns dabei, Fehler frühzeitig zu entdecken und zu beheben, bevor sie in der Produktion auftreten. Durch das Schreiben von Tests können wir sicherstellen, dass unsere Software wie erwartet funktioniert und potenzielle Probleme vermeiden.

## Wie man Tests schreibt

Um Tests in Rust zu schreiben, verwenden wir das in der Standardbibliothek enthaltene Modul `test`. Hier ist ein einfaches Beispiel, wie wir eine Funktion `add` testen können, die zwei Zahlen addiert:

```Rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}

// Testfunktion mit dem Präfix `test` und der Annotation `#[test]`
#[test]
fn test_add() {
    // Wir rufen die Funktion mit verschiedenen Eingaben auf und überprüfen das erwartete Ergebnis
    assert_eq!(add(2, 3), 5);
    assert_eq!(add(-1, 5), 4);
    assert_eq!(add(0, 0), 0);
}
```

Beim Ausführen dieser Testfunktion erhalten wir folgende Ausgabe:

```
running 1 test
test test_add ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Wie wir sehen können, wurde unser Test erfolgreich durchgeführt und alle Assertions sind wahr. Wenn jedoch eine Assertion fehlschlägt, erhalten wir einen ausführlicheren Fehlerbericht, der uns dabei unterstützt, den Fehler in unserer Funktion zu finden.

## Tiefer ins Detail gehen

Das Schreiben von Tests ist jedoch weit mehr als nur das einfache Überprüfen von erwarteten Ergebnissen. Durch das Verständnis der vielfältigen Möglichkeiten von Tests können wir unsere Tests effektiver gestalten.

Ein wichtiger Aspekt ist die Verwendung von Test-Driven Development (TDD). Dabei schreiben wir zuerst den Test für eine Funktion, bevor wir diese implementieren. Dies kann uns dabei helfen, eine klarere Vorstellung von der Funktion zu erhalten und mögliche Anforderungen oder Probleme frühzeitig zu erkennen.

Ein weiteres nützliches Tool ist das Modul `assert`, das uns ermöglicht, eigene Assertions zu erstellen und so spezifische Bedingungen zu überprüfen. Des Weiteren bieten sich auch Associations-Tests an, bei denen wir das Verhalten unserer Funktion mit zufälligen Eingaben und Ausgaben überprüfen.

Es gibt noch viele weitere Möglichkeiten und Techniken, um Tests zu schreiben. Mit dem Verständnis für diese verschiedenen Ansätze können wir unsere Tests optimieren und die Qualität unserer Software verbessern.

## Siehe auch

- [Rust Dokumentation zu dem Modul `test`](https://doc.rust-lang.org/stable/rust-by-example/testing/unit_testing.html)
- [Offizielles Rust Book zu Tests](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rustlings Übungseinheit zu Tests](https://github.com/rust-lang/rustlings/tree/main/exercises/testing)