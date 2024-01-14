---
title:                "C: Tests schreiben"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests schreiben ist ein wichtiger Bestandteil des Programmierens. Es ermöglicht uns, sicherzustellen, dass unser Code korrekt funktioniert und potenzielle Bugs frühzeitig zu finden. Es reduziert auch die Zeit, die wir für das Debuggen und Fehlerbehebung aufwenden müssen, was letztendlich zu einer effizienteren Programmierung führt.

## Wie funktioniert es?

Um Tests in C zu schreiben, verwenden wir das Test-Framework "Unit Testing Framework" (UTF). Zuerst müssen wir unser Testprogramm mit ``#include <utf.h>`` importieren. Dann können wir unsere Testfunktionen mit dem Makro `TEST()` definieren und mit verschiedenen Assert-Funktionen wie `ASSERT_EQUAL()` und `ASSERT_NOT_EQUAL()` überprüfen, ob unsere erwarteten Ergebnisse mit den tatsächlichen übereinstimmen.

Hier ist ein Beispiel eines einfachen Tests, der überprüft, ob die Funktion `add()` korrekt zwei Zahlen addiert:

```
C
TEST(test_addition) {
  int result = add(3, 5);
  ASSERT_EQUAL(result, 8);
}
```

Wenn wir jetzt unser gesamtes Testprogramm ausführen, sollten wir sehen, dass unser Test erfolgreich ist und keine Fehler aufgetreten sind.

## Tiefere Einblicke

Es gibt verschiedene Arten von Tests, die wir beim Schreiben von C-Code verwenden können, wie zum Beispiel Unit Tests, Integrationstests und Systemtests. Unit Tests überprüfen einzelne Funktionen oder Module, während Integrationstests die Interaktion zwischen verschiedenen Modulen testen. Systemtests prüfen schließlich das gesamte Programm als Ganzes.

Um sicherzustellen, dass wir effektive Tests schreiben, müssen wir auch darauf achten, dass unsere Tests unabhängig, reproduzierbar und tolerant gegenüber Änderungen sind. Unabhängigkeit bedeutet, dass jeder Test unabhängig von anderen getestet werden sollte. Reproduzierbarkeit stellt sicher, dass unsere Tests bei jeder Ausführung dieselben Ergebnisse liefern. Und Toleranz gegenüber Änderungen bedeutet, dass unsere Tests auch dann erfolgreich sind, wenn sich der Code ändert.

## Siehe auch

- [Einführung in das Unit Testing in C](https://www.tutorialspoint.com/cprogramming/c_unit_testing.htm)
- [CUnit Dokumentation](http://cunit.sourceforge.net/doc/index.html)
- [The All-Pair Testing Tool (APT-Engine)](https://www.tutorialspoint.com/software_testing_dictionary/all_pair_testing_tool.htm)