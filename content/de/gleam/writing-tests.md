---
title:                "Tests schreiben"
html_title:           "Gleam: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Was ist es und warum?
Tests schreiben ist ein wichtiger Teil des Programmierens. Es ermöglicht Programmierern, die Funktionen ihres Codes zu überprüfen und sicherzustellen, dass er wie erwartet funktioniert.

## Wie geht es?
Es gibt verschiedene Ansätze zum Schreiben von Tests in Gleam. Das grundlegende ist die Verwendung der `assert`-Funktion, um Behauptungen über das erwartete Verhalten des Codes zu machen. Zum Beispiel:
```
🍎 true_case =
    assert.true 2 * 2 == 4

🍏 false_case =
    assert.true 2 * 2 == 5
```
Die `assert`-Funktion erwartet eine boolesche Aussage und gibt eine Fehlermeldung aus, wenn sie falsch ist. Im `true_case`-Beispiel ist die Behauptung wahr, also gibt es keine Fehlermeldung. Im `false_case`-Beispiel ist die Behauptung falsch, also würde eine Fehlermeldung ausgegeben werden, die angibt, dass 4 nicht gleich 5 ist.

## Tief eintauchen
Das Schreiben von Tests ist ein wichtiger Bestandteil von Test-getriebener Entwicklung (TDD). Dabei schreiben Programmierer Tests, bevor sie den eigentlichen Code schreiben, um sicherzustellen, dass er wie erwartet funktioniert. Neben `assert` gibt es auch weitere Möglichkeiten zum Schreiben von Tests in Gleam, wie beispielsweise die Verwendung von Modulen oder die Verwendung von Test-Frameworks wie `gleam_pest`.

## Sieh dir auch an