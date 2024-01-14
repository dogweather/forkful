---
title:    "C: Tests schreiben"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-tests.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie C-Code schreiben, kann es verlockend sein, Tests zu überspringen und sich auf das Schreiben von Funktionen und Programmen zu konzentrieren. Tests können jedoch eine wichtige Rolle in der Entwicklung eines robusten und zuverlässigen Codes spielen. Sie bieten eine Möglichkeit, Fehler zu finden und zu beheben, bevor sie zu größeren Problemen führen.

## Wie
Um Tests für Ihren C-Code zu schreiben, können Sie die integrierte Unit Testing-Bibliothek *ctest* verwenden. Hier ist ein Beispiel, wie Sie eine einfache Funktion für das Multiplizieren von Zahlen testen können:

```C
#include <stdio.h>
#include <stdlib.h>
#include <ctest.h>

int multiply(int a, int b) {
  return a * b;
}

CTEST(multiply_test, result_equals_8) {
  int result = multiply(2, 4);
  ASSERT_EQUAL(8, result);
}
```

Der obige Code importiert die *ctest*-Bibliothek und definiert eine Funktion zur Multiplikation von Zahlen. Dann wird ein Test erstellt, der überprüft, ob das Ergebnis der Multiplikation von 2 und 4 gleich 8 ist. Mit dem Befehl `ctest` wird der Test ausgeführt und Sie sollten die Ausgabe `ALL OK (1 test)` sehen.

Sie können auch Tests für fehlerhafte Eingaben schreiben, um sicherzustellen, dass Ihr Code robust ist. Hier ist ein Beispiel für einen Test, der sicherstellt, dass die Funktion *multiply* eine Fehlermeldung ausgibt, wenn einer der Eingabewerte 0 ist:

```C 
#include <stdio.h>
#include <stdlib.h>
#include <ctest.h>

CTEST(multiply_test, input_error) {
  int result = multiply(6, 0);
  ASSERT_STR("Error: Input values cannot be 0", result);
}
```

Auch hier sollten Sie eine erfolgreiche Ausgabe `ALL OK (1 test)` erhalten, da in diesem Fall der Test erfolgreich ist, wenn eine Fehlermeldung ausgegeben wird.

## Deep Dive
Tests sind ein wichtiger Bestandteil des Test-driven Development (TDD) Prozesses. Sie können auch als zusätzliche Sicherheit dienen, wenn Sie Änderungen an Ihrem Code vornehmen oder neue Funktionen hinzufügen. Durch das Schreiben von Tests sorgen Sie dafür, dass alle wichtigen Teile Ihrer Software richtig funktionieren, bevor Sie sie in der Praxis einsetzen.

Darüber hinaus kann das Schreiben von Tests dazu beitragen, Ihren Code besser zu organisieren und einfacher zu warten. Sie können beispielsweise Tests für bestimmte Funktionen oder Module schreiben, um sicherzustellen, dass sie nicht durch unerwartete Änderungen in anderen Teilen Ihres Codes beeinflusst werden.

## Siehe auch
- [Introduction to Test-driven Development (Deutsche Version)](https://wiki.c2.com/?TestDrivenDevelopmentInEinemBild)
- [Die offizielle ctest Dokumentation](https://cmake.org/cmake/help/latest/manual/ctest.1.html)
- [Tutorial: C-Testing mit ctest](https://www.jeremymorgan.com/tutorials/c-programming/how-to-test-c-with-ctest/)