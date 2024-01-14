---
title:                "Gleam: Testen schreiben"
simple_title:         "Testen schreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Tests ist ein wesentlicher Bestandteil jeder Entwicklungsarbeit. Es stellt sicher, dass der Code zuverlässig funktioniert und mögliche Fehler frühzeitig erkannt werden können. Außerdem kann es dabei helfen, zukünftige Änderungen oder Erweiterungen einfacher und sicherer durchzuführen.

## Wie geht man vor?

Die Testfunktionen in Gleam werden mit dem ```gleam_test``` Modul erstellt. Zum Beispiel könnte eine Funktion zum Testen einer einfachen Addition folgendermaßen aussehen:

``` gleam_test
Additionstest-funktion() {
  assert_equal(2, 1 + 1)
}
```

Der Befehl ```assert_equal``` wird verwendet, um zwei Werte zu vergleichen und sicherzustellen, dass sie gleich sind. In diesem Fall wird überprüft, ob die Addition von 1 und 1 tatsächlich 2 ergibt. Ist dies nicht der Fall, wird ein Fehler angezeigt.

Darüber hinaus können auch Fehlerfälle getestet werden, indem ```gleam_test``` Funktionen mit dem ```test_case``` Makro erstellt werden. Hier ein Beispiel für eine Funktion, die sicherstellt, dass eine Division durch Null einen Fehler auslöst:

``` gleam_test
Division durch Null () {
  test_case("Sollte einen Fehler werfen") {
    assert_error(_, 1 / 0)
  }
}
```

Wie man sehen kann, ermöglicht es Gleam, präzise und effektive Tests zu schreiben, um die Funktionalität des Codes zu überprüfen.

## Tieferer Einblick

Zusätzlich zu den grundlegenden Funktionen für das Testen von Werten und Fehlern bietet Gleam auch die Möglichkeit, bestimmte Eigenschaften zu überprüfen. Hierfür kann das Modul ```gleam_quickcheck``` verwendet werden. Mit QuickCheck können Eigenschaften des Codes überprüft werden, indem zufällige Eingabedaten generiert und geprüft werden, ob die Ausgabe den erwarteten Eigenschaften entspricht.

Außerdem können Gleam Testfunktionen auch mit bestimmten Bedingungen oder Annahmen versehen werden, um noch genauere Überprüfungen durchzuführen. Dies macht es möglich, komplexe Anforderungen an den Code zu testen und sicherzustellen, dass er in allen möglichen Szenarien funktioniert.

## Siehe auch

- Offizielle Dokumentation zu Tests in Gleam: https://gleam.run/book/testing.html
- Einführung in QuickCheck für Gleam: https://www.notion.so/gleamrun/QuickCheck-b36b20e11d8f4ec7b60b7e10d518e607
- Beispielprojekt mit umfassenden Tests in Gleam: https://github.com/gleam-lang/gleam_binary_tree