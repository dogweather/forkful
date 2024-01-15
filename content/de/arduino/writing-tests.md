---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil des Programmierens mit Arduino. Sie helfen dabei, Fehler im Code zu erkennen und zu beheben, bevor sie zu größeren Problemen führen. Außerdem können sie dabei helfen, die Zuverlässigkeit und Funktionalität eines Projekts zu verbessern.

## Wie geht es

Um Tests in deine Arduino-Programme zu implementieren, musst du zunächst die Bibliothek "ArduinoUnit" installieren. Diese Bibliothek stellt verschiedene Funktionen und Methoden bereit, die dir dabei helfen, Tests zu schreiben und auszuführen.

Als nächstes musst du deine Tests in separate Dateien oder Funktionen schreiben. Diese Dateien oder Funktionen sollten mit dem Präfix "test_" beginnen, damit die Bibliothek sie als Tests erkennen kann. Beispiel:

```Arduino
#include <ArduinoUnit.h>

test_schaltplan() {
  schaltplan.anfangsKapazität();
  assertEqual(12, schaltplan.spannung);
}

test_antrieb() {
  // Teste die Geschwindigkeit des Antriebs
  assertGreater(antrieb.geschwindigkeit, 0);
}
```

Im obigen Beispiel werden zwei Tests erstellt: "schaltplan" und "antrieb". In jedem Test werden verschiedene Funktionen und Methoden aufgerufen und ihre Ergebnisse mit der "assert"-Funktion überprüft.

Nachdem du deine Tests geschrieben hast, kannst du sie mithilfe des Arduino-Boards ausführen und die Ergebnisse in der seriellen Monitor-Ansicht überprüfen. Beispiel:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Führe alle Tests aus
  Test::run();
}
```

## Tiefer eintauchen

Um deine Tests noch effektiver zu machen, kannst du auch verschiedene Parameter und Optionen in der "assert"-Funktion verwenden. Diese ermöglichen es dir, genauere Aussagen zu treffen und spezifische Fehlermeldungen zu generieren. Beispiel:

```Arduino
assertEqual[message](expected, actual[, tolerance])
assertNotEqual[message](expected, actual[, tolerance])
assertTrue[message](expression)
assertFalse[message](expression)
assertGreaterThan[message](a, b)
assertGreaterThanOrEqual[message](a, b)
assertLessThan[message](a, b)
assertLessThanOrEqual[message](a, b)
```

Zusätzlich dazu kannst du auch spezielle Funktionen wie "before()" und "after()" verwenden, um vor und nach jedem Test bestimmte Aufgaben auszuführen, z.B. das Initialisieren von Variablen oder das Drucken von Debugging-Informationen.

## Siehe auch

- [Offizielle ArduinoUnit-Dokumentation] (https://github.com/mmurdoch/arduinounit)
- [Tutorial zur Verwendung von Tests in Arduino] (https://www.arduino.cc/reference/en/libraries/arduinounit/)
- [Video: Wie man Tests mit Arduino schreibt] (https://www.youtube.com/watch?v=iDaTDpUkSUQ)