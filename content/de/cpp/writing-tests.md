---
title:                "C++: Tests schreiben"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Testen ist ein wesentlicher Bestandteil der Softwareentwicklung, da es sicherstellt, dass der Code korrekt und zuverlässig funktioniert. Durch das Schreiben von Tests können potenzielle Fehler erkannt und behoben werden, bevor die Software in die Produktion geht, was Zeit und Ressourcen sparen kann. Es ist daher wichtig, diese Praktik zu beherrschen und in jedes Softwareprojekt zu integrieren.

## Wie man Tests schreibt

Das Schreiben von Tests kann mit Hilfe von Frameworks wie dem C++ Unit Test Framework oder dem Google Test Framework durchgeführt werden. Hier ist ein Beispiel für eine Testklasse, die mithilfe des Google Test Frameworks geschrieben wurde:

```C++
#include <gtest/gtest.h>

// Definition der Testklasse
class SampleTest : public ::testing::Test {
protected:

  // Vorbereitung von Testdaten oder Zuständen
  void SetUp() override {}

  // Aufraeumen nach dem Test
  void TearDown() override {}
};

// Testfunktion
TEST_F(SampleTest, TestErfolg) {
  EXPECT_EQ(2 + 2, 4);
}

TEST_F(SampleTest, TestFehlschlag) {
  EXPECT_EQ(2 + 2, 5);
}

// Start des Tests
int main(int argc, char* argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

Das obige Beispiel zeigt die grundlegenden Strukturen von Unit Tests. Die Testklasse erbt von der Basisklasse "testing::Test" und hat Funktionen für die Testvorbereitung und das Aufräumen. Jede Testfunktion innerhalb der Klasse wird mit dem Makro "TEST_F" deklariert, gefolgt vom Namen der Klasse und dem Testnamen. Innerhalb der Testfunktion werden Assertions verwendet, um das erwartete Verhalten des Codes zu überprüfen. Wenn alle Tests erfolgreich sind, gibt der letzte Teil des Codes (die "main"-Funktion) eine "0" zurück und zeigt somit an, dass alle Tests erfolgreich waren.

## Tiefere Einblicke

Beim Schreiben von Tests gibt es verschiedene Konzepte und Techniken, die berücksichtigt werden sollten. Dazu gehören zum Beispiel die Verwendung von Test-Driven Development (TDD) und die Erstellung von "testbaren" Code. Darüber hinaus ist es wichtig, zu verstehen, wie man effektive Testfälle erstellt und wie man Fehlermeldungen interpretiert, um Fehler schnell und effizient zu beheben.

## Siehe auch

- [C++ Unit Test Framework](https://github.com/cpp-testing/Catch2)
- [Google Test Framework](https://github.com/google/googletest)
- [Test-Driven Development (TDD)](https://de.wikipedia.org/wiki/Testgetriebene_Entwicklung)