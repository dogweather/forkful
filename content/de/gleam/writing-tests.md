---
title:    "Gleam: Testen schreiben"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Sich Tests zu schreiben kann oft zeitaufwendig und mühsam erscheinen, aber es ist ein wichtiger Schritt, um sicherzustellen, dass der Code richtig funktioniert und robust ist. Durch das Schreiben von Tests können potenzielle Fehler frühzeitig erkannt werden, was Zeit und Mühe in der späteren Entwicklungsphase spart. Außerdem erhöht das Schreiben von Tests die Qualität des Codes und erleichtert die Wartung und Erweiterung in der Zukunft.

## Wie geht man vor

Die Tests in Gleam werden mit der eingebauten Bibliothek `gleam_test` geschrieben. Hier ist ein Beispiel eines einfachen Tests, der überprüft, ob die `double`-Funktion korrekt funktioniert:

```
Gleam beispiel

import gleam_test

fn double(x) {
  x * 2
}

test "doppelte Funktion verdoppelt die Zahl" {
  assert double(4) == 8
}

// Ausgabe: `✓ doppelte Funktion verdoppelt die Zahl`
```

Wie man sehen kann, wird eine Test-Funktion mit dem Schlüsselwort `test` erstellt und die zu überprüfende Bedingung mit dem Schlüsselwort `assert` festgelegt. Die Ausgabe des Tests wird mit dem grünen Checkmark angezeigt. Sobald alle Tests erfolgreich durchlaufen sind, wird der gesamte Testvorgang als erfolgreich markiert.

Um mehr über das Schreiben von Tests in Gleam zu erfahren, können Sie sich die offizielle Dokumentation ansehen: [Gleam Tests schreiben](https://gleam.run/book/tour/tests.html)

## Tiefergehende Informationen

Es gibt noch einige weitere Funktionen und Möglichkeiten bei der Erstellung von Tests in Gleam. Hier ein paar Tipps:

- Um auf einen bestimmten Test auszuführen, kann der Befehl `gleam test [Testname]` verwendet werden.
- Gleam bietet die Möglichkeit, verschiedene "`"Output`" zu testen, z.B. `assert string(1) == "1"`.
- Es können auch Tests für Ausnahmen bzw. Fehler erstellt werden, um sicherzustellen, dass der Code korrekt auf ungültige Eingaben reagiert.

Für detaillierte Informationen und Beispiele können Sie sich die offizielle Gleam-Dokumentation ansehen: [Tests und Matches in Gleam](https://gleam.run/book/standard-library.html#tests-and-matches).

## Siehe auch

- [Gleam-Dokumentation](https://gleam.run/)
- [Gleam auf GitHub](https://github.com/gleam-lang/gleam)
- [Einführung in Gleam für funktionale Programmierer](https://dennisreimann.de/articles/visual-introduction-to-gleam.html)