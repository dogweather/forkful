---
title:                "C++: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Warum Tests schreiben?

Tests sind ein wichtiger Bestandteil des Softwareentwicklungsprozesses und haben viele Vorteile. Durch das Schreiben von Tests kann man sicherstellen, dass der Code korrekt funktioniert und Bugs frühzeitig erkannt werden. Außerdem ermöglichen Tests Entwicklern, Änderungen am Code vorzunehmen, ohne die Funktionalität der Software zu beeinträchtigen. Das spart Zeit und Ressourcen und sorgt für eine höhere Qualität des Codes. In diesem Blogbeitrag werden wir uns genauer mit dem Schreiben von Tests in C++ befassen.

## Wie man Tests in C++ schreibt

Um Tests in C++ zu schreiben, benötigt man eine Test-Framework-Software wie zum Beispiel Google Test oder Catch2. Diese bieten Möglichkeiten, Tests zu erstellen und auszuführen. Hier ist ein einfaches Beispiel für einen Test mit Google Test:

```C++
#include <gtest/gtest.h>

TEST(Math, Addition) {
  EXPECT_EQ(2+2, 4);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

In diesem Beispiel testen wir die Addition von zwei Zahlen und erwarten, dass das Ergebnis 4 ist. Wenn der Test erfolgreich ist, wird "OK" ausgegeben, andernfalls wird eine Fehlermeldung angezeigt.

## Tieferer Einblick ins Schreiben von Tests

Beim Schreiben von Tests ist es wichtig, verschiedene Testfälle zu berücksichtigen und sicherzustellen, dass alle möglichen Szenarien getestet werden. Außerdem sollten die Tests einfach verständlich und wartbar sein, damit sie von anderen Entwicklern leicht überprüft und aktualisiert werden können. Es ist auch ratsam, Tests automatisiert auszuführen, um sicherzustellen, dass sie bei jeder Änderung am Code durchgeführt werden und die Software immer korrekt bleibt.

Eine wichtige Strategie beim Schreiben von Tests ist die "Red-Green-Refactor"-Methode. Zuerst schreibt man einen Test, der fehlschlägt (rot), dann schreibt man den Code, der den Test bestehen lässt (grün) und anschließend optimiert man den Code, um sicherzustellen, dass er effizient und gut strukturiert ist (Refaktor).

## Siehe auch

* [Google Test Dokumentation](https://github.com/google/googletest/blob/master/googletest/docs/)
* [Catch2 Tutorial](https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md)
* [Effektive Tests schreiben in C++](https://accu.org/index.php/journals/2566)