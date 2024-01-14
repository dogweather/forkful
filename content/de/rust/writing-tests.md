---
title:    "Rust: Testen schreiben."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Welt der Softwareentwicklung wird es immer wichtiger, qualitativ hochwertige und fehlerfreie Software zu liefern. Das Schreiben von Tests ist ein unverzichtbarer Schritt, um sicherzustellen, dass unsere Programme zuverlässig und robust sind. Auch in der Rust-Programmierung sollten Tests nicht vernachlässigt werden. In diesem Blogbeitrag werden wir uns genauer ansehen, warum das Schreiben von Tests in Rust von großer Bedeutung ist.

## Wie geht man vor?

Zunächst müssen wir sicherstellen, dass wir die notwendigen Abhängigkeiten für das Testen in unserem Projekt haben. Dazu gehören das `rustc` und das `test`-Paket. Sobald diese installiert sind, können wir unsere Tests schreiben.

In Rust können wir Tests in derselben Datei wie unser Code schreiben, was es einfach und übersichtlich macht. Wir verwenden die Makros `#[cfg(test)]` und `#[test]`, um unsere Tests zu kennzeichnen. Innerhalb des `#[test]`-Makros können wir unsere Testfunktionen schreiben und Assertions hinzufügen, um sicherzustellen, dass unsere Ergebnisse den erwarteten Werten entsprechen.

```Rust
#[cfg(test)]
mod tests {
   #[test]
   fn test_addition() {
       let result = 2 + 2;
       assert_eq!(result, 4);
   }
}
```

Um unsere Tests auszuführen, können wir das `cargo test`-Kommando verwenden. Dies wird alle Dateien mit `#[test]`-Makros im `tests`-Ordner ausführen und uns mitteilen, ob die Tests bestanden oder fehlgeschlagen sind. Wenn wir mehr Informationen über die einzelnen Tests erhalten möchten, können wir auch das `--verbose`-Flag hinzufügen.

## Tiefergehende Erläuterung

Es gibt verschiedene Arten von Tests in Rust, die wir schreiben können, wie z.B. Modultests und Integrationstests. Wir können auch das `should_panic`-Attribut verwenden, um sicherzustellen, dass eine bestimmte Funktion einen Fehler wirft. Darüber hinaus ermöglicht uns Rust durch die Verwendung von generischen Funktionen und Traits das Schreiben von bulkigen Tests, die für mehrere Datentypen und Strukturen funktionieren.

Das Schreiben von Tests in Rust ist auch eine großartige Möglichkeit, um mehr über die Sprache und ihre Funktionen zu lernen, da es uns dazu zwingt, unsere Funktionen und Datenstrukturen besser zu verstehen und ihre Grenzen zu testen.

## Siehe auch

- [The Rust Book: Writing Automated Tests](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rust by Example: Testing](https://doc.rust-lang.org/stable/rust-by-example/testing.html)
- [Official Rust Testing Guide](https://www.rust-lang.org/learn/testing)