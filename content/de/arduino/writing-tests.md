---
title:                "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Testen ist ein wichtiger Bestandteil der Softwareentwicklung, um sicherzustellen, dass unser Code fehlerfrei funktioniert und die erwarteten Ergebnisse liefert. Daher ist es für jeden, der mit Arduino-Programmierung beschäftigt ist, unerlässlich, Tests zu schreiben.

## Wie geht man vor?

Die Arduino-Umgebung bietet einfache und effektive Möglichkeiten, Tests für unsere Projekte zu schreiben. Hier ist ein Beispiel, wie wir eine Funktion testen können, die zwei Zahlen addiert:

```Arduino
int addieren(int a, int b) {
  return a + b;
}

int result = addieren(5, 10);
Serial.println(result) // Ausgabe: 15
```

In diesem Beispiel haben wir eine Funktion definiert, die zwei ganze Zahlen als Parameter annimmt und sie addiert. Dann rufen wir die Funktion auf und geben die Ausgabe auf dem Seriellen Monitor aus. Wir können jetzt eine Testfunktion für diese Funktion schreiben:

```Arduino
void testAddieren() {
  int result = addieren(5, 10);
  if (result == 15) {
    Serial.println("Test erfolgreich!"); // Ausgabe: Test erfolgreich!
  } else {
    Serial.println("Test fehlgeschlagen!");
  }
}
```

In dieser Testfunktion überprüfen wir, ob die Ausgabe der Funktion `addieren` korrekt ist und geben entsprechend eine Erfolgsmeldung oder eine Fehlermeldung aus. So können wir sicherstellen, dass unsere Funktion ordnungsgemäß funktioniert.

## Tiefere Einblicke

Es gibt verschiedene Arten von Tests, die wir für unsere Arduino-Projekte schreiben können, wie z.B. Einheitstests, Integrationstests und Funktionstests. Ein weiteres wichtiges Konzept ist das Test Driven Development (TDD), bei dem Tests vor dem eigentlichen Code geschrieben werden. Dadurch wird sichergestellt, dass der Code qualitativ hochwertig und fehlerfrei ist, bevor er in die Anwendung integriert wird.

Außerdem ist es wichtig, unsere Tests regelmäßig auszuführen und zu aktualisieren, um sicherzustellen, dass sie immer gültig bleiben und eventuelle Änderungen im Code berücksichtigen.

## Siehe auch

- [Arduino-Testbibliothek](https://www.arduino.cc/en/Reference/ArduinoUnit)
- [Tutorial: Test Driven Development mit Arduino](https://www.itead.cc/blog/tutorials/tutorial-automated-testing-arduino)
- [Tutorial: Einheitstests mit Arduino](https://lastminuteengineers.com/unit-testing-arduino-tdd-unit-test/)