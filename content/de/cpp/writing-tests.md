---
title:    "C++: Tests schreiben"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Tests ist ein wichtiger Bestandteil des Programmierens. Es hilft Programmierern dabei, sicherzustellen, dass ihr Code korrekt und stabil ist. Tests können auch dabei helfen, Bugs frühzeitig zu erkennen und zu beheben, was letztendlich zu einer höheren Qualität des Codes führt.

## Wie geht man vor

Um Tests in C++ zu schreiben, verwendet man normalerweise eine Unit-Testing-Bibliothek wie zum Beispiel Catch2. Zunächst muss man eine Test-Datei erstellen und die Bibliothek in das Projekt einbinden. Dann kann man verschiedene Testfälle definieren und überprüfen, ob die erwarteten Ergebnisse erzielt werden.

Ein Beispiel dafür wäre die Addition von zwei Zahlen. Innerhalb einer ```C++ TEST_CASE``` Funktion kann man die Addition durchführen und mit Hilfe von ```REQUIRE``` sicherstellen, dass das Ergebnis korrekt ist. Der Output sollte dann folgendermaßen aussehen:

```
TEST_CASE("Additionstest") {

  int sum = add(5, 10); // Funktion "add" muss implementiert werden
  REQUIRE(sum == 15); // Überprüft, ob das Ergebnis 15 ist
}
```

Dies ist nur ein einfaches Beispiel, aber es zeigt, wie man mit Unit-Tests arbeiten kann. Es gibt noch viele weitere Möglichkeiten, wie man Tests schreiben und ausführen kann, aber das grundlegende Konzept bleibt das Gleiche.

## Tiefere Einblicke

Tests können auch in sogenannten Test-Driven-Development (TDD) Ansätzen verwendet werden. Hierbei schreibt man zuerst den Test und implementiert dann den Code, um den Test erfolgreich zu bestehen. Dies stellt sicher, dass der Code immer zuerst getestet wird, bevor er in die Anwendung integriert wird.

Außerdem ist es wichtig, verschiedene Arten von Tests zu kennen und zu verwenden, wie z.B. Unit-Tests, Integrationstests und End-to-End-Tests. Jede Art hat ihre eigenen Vorteile und hilft dabei, unterschiedliche Aspekte des Codes zu überprüfen.

Insgesamt ist das Schreiben von Tests eine wichtige Praxis für alle Programmierer, um sicherzustellen, dass ihr Code fehlerfrei und stabil ist. Es erfordert zwar zunächst etwas zusätzlichen Aufwand, aber es spart letztendlich Zeit und vermeidet mögliche Probleme in der Zukunft.

## Siehe auch

- [Catch2 Dokumentation](https://github.com/catchorg/Catch2/tree/devel/docs)
- [Tutorial zu Unit-Tests in C++](https://www.codingame.com/playgrounds/247/introduction-to-unit-testing-with-catch)
- [Test-Driven-Development Einführung](https://www.geeksforgeeks.org/test-driven-development-tdd/)
- [Arten von Tests in der Softwareentwicklung](https://www.testlodge.com/types-of-software-testing/)