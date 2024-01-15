---
title:                "Testen schreiben"
html_title:           "C++: Testen schreiben"
simple_title:         "Testen schreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil der Softwareentwicklung. Sie sind ein effektives Werkzeug, um sicherzustellen, dass der Code funktioniert, wie er sollte und um mögliche Fehler frühzeitig zu erkennen. In diesem Artikel werde ich erklären, warum es wichtig ist, Tests zu schreiben und wie man Tests in C++ schreibt.

## So geht's

Um Tests in C++ zu schreiben, verwenden wir die Bibliothek "gtest". Diese Bibliothek bietet eine einfache und effektive Möglichkeit, Unit-Tests zu schreiben. Zunächst müssen wir die Bibliothek in unserem Projekt einbinden. Dafür müssen wir das "gtest" Repository auf Github herunterladen und die Dateien in unserem Projektordner speichern.

Anschließend erstellen wir eine Testklasse, die von der Klasse "testing::Test" erbt. In dieser Klasse können wir dann unsere Tests schreiben, indem wir die Funktionen "TEST" und "EXPECT_EQ" verwenden. Beispielcode sieht folgendermaßen aus:

```C++
#include "gtest/gtest.h"  // Einbinden der Bibliothek

// Erstellen einer Testklasse
class TestClass : public testing::Test {
  // Hier können weitere Funktionen oder Variablen erstellt werden
};

// Schreiben eines Tests
TEST(TestClass, AddTest) {
  int result = 2 + 2;
  // Überprüfung des erwarteten Ergebnisses mit EXPECT_EQ
  EXPECT_EQ(result, 4);
}
```

Nachdem wir alle unsere Tests geschrieben haben, können wir sie ausführen und das Ergebnis überprüfen. Dafür müssen wir in unserer "main" Funktion die Funktion "RUN_ALL_TESTS()" aufrufen. Wenn alle Tests erfolgreich durchlaufen wurden, wird "OK" ausgegeben. Ansonsten wird das fehlgeschlagene Testergebnis angezeigt. Beispielcode für die "main" Funktion:

```C++
#include "gtest/gtest.h"

int main(int argc, char **argv) {
  // Initialisierung und Ausführung der Tests
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

Das Ergebnis unserer Tests wird dann in der Konsole ausgegeben. Bei einem erfolgreichen Test wird "OK" angezeigt, ansonsten werden Details über den fehlgeschlagenen Test ausgegeben.

## Tiefergehende Informationen

In diesem Artikel haben wir gezeigt, wie man Tests in C++ mit der "gtest" Bibliothek schreibt. Es gibt jedoch noch viele weitere Möglichkeiten und Funktionen, die "gtest" bietet, um unsere Tests noch effektiver zu gestalten. Es lohnt sich also, sich etwas tiefer in die Bibliothek einzuarbeiten und ihre verschiedenen Funktionen zu nutzen.

## Siehe auch

- [Offizielle Dokumentation von "gtest"](https://github.com/google/googletest/)
- [Tutorial für "gtest"](https://www.tutorialspoint.com/googletest/index.htm)
- [Weitere Tipps für das Schreiben von effektiven Tests](https://www.codeproject.com/Articles/15985/Introduction-to-Google-Test-GTest-for-Cplusplus)