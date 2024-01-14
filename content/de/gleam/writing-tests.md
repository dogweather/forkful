---
title:    "Gleam: Tests schreiben"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Das Testen von Code ist eine wichtige Praxis, die dabei hilft, die Funktionalität und Zuverlässigkeit von Software zu verbessern. Es ermöglicht Entwicklern, potenzielle Fehler und Schwachstellen frühzeitig zu erkennen und zu beheben, was letztendlich zu einer besseren Nutzererfahrung führt.

## Wie man Tests in Gleam schreibt

Um Tests in Gleam zu schreiben, müssen wir zunächst das `gleam/test` Paket importieren. Anschließend können wir Testfunktionen mit dem `test`-Makro erstellen und innerhalb dieser Funktionen unsere Testfälle mit Assertions definieren.

```Gleam
import gleam/test

pub fn my_test() {
  assert.equal(2, 1 + 1)
}
```

Wenn wir nun unsere Tests ausführen, erhalten wir die folgende Ausgabe:

```
my_test: PASS
```

## Tiefgreifende Einblicke

Tests ermöglichen es uns, verschiedene Szenarien zu simulieren und sicherzustellen, dass unser Code in allen Fällen ordnungsgemäß funktioniert. Es ist wichtig, Tests für jede Funktion und jedes Modul zu schreiben, um sicherzustellen, dass Änderungen oder Updates keine unerwarteten Auswirkungen haben.

Es ist auch ratsam, das Arrange-Act-Assert Muster zu verwenden, um unsere Tests klar zu strukturieren. Dies bedeutet, dass wir zuerst die erforderlichen Vorbedingungen für unseren Test definieren, dann die Aktion durchführen, die wir testen möchten, und schließlich überprüfen, ob das Ergebnis unseren Erwartungen entspricht.

## Siehe auch

- [Offizielle Gleam Dokumentation zum Testen](https://gleam.run/book/testing.html)
- [Tutorial: Gleam Testen für Anfänger](https://medium.com/@matthewphilleo/getting-started-with-gleam-testing-for-beginners-3d5de14a9f6b)
- [Gleam Testen mit dem `unit_test` Paket](https://davidgom.github.io/tdd-with-gleam/)