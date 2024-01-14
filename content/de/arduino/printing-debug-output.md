---
title:                "Arduino: Ausgabe von Debug-Meldungen"
simple_title:         "Ausgabe von Debug-Meldungen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren mit Arduino kann es manchmal schwierig sein, Fehler zu finden oder zu verstehen, was das Programm gerade tut. Das Drucken von Debug-Ausgaben kann dabei helfen, diese Probleme zu lösen und das Verständnis für den Code zu verbessern.

## How To

Um Debug-Ausgaben zu drucken, können wir die Funktion `Serial.print()` verwenden. Diese Funktion nimmt eine Variable oder einen Text als Parameter und gibt ihn über die serielle Verbindung an den Computer aus.

Um dies in der Praxis zu sehen, schauen wir uns ein einfaches Beispiel an:

```Arduino

int zahl = 42; // Eine beliebige Zahl definieren
Serial.begin(9600); // Serielle Verbindung starten
Serial.print("Die Zahl ist: "); // Text ausgeben
Serial.println(zahl); // Variable ausgeben
```

Wenn wir dieses Beispiel ausführen, sehen wir in der seriellen Monitor-Ausgabe `Die Zahl ist: 42`. Wenn wir `Serial.print()` anstelle von `Serial.println()` verwenden, wird die Variable ohne Zeilenumbruch ausgegeben.

## Deep Dive

Es gibt viele verschiedene Möglichkeiten, Debug-Ausgaben zu nutzen. Zum Beispiel können wir `Serial.print()` verwenden, um den aktuellen Wert von Variablen während der Ausführung des Programms zu überwachen. Dies kann besonders hilfreich sein, wenn wir mit Sensoren oder anderen externen Geräten arbeiten. Wir können auch `Serial.print()` nutzen, um zu überprüfen, ob unser Code bestimmte Bedingungen erfüllt und gegebenenfalls Fehlermeldungen ausgeben.

Eine weitere hilfreiche Funktion ist `Serial.begin()`, mit der wir die Geschwindigkeit der seriellen Verbindung einstellen können. Standardmäßig ist diese auf 9600 Baud eingestellt, aber je nach Anwendungsfall kann es sinnvoll sein, diese anzupassen.

Es ist wichtig zu beachten, dass Debug-Ausgaben nur während der Entwicklung und zum Zweck der Fehlerbehebung verwendet werden sollten. Wenn das Programm fertig ist, sollten diese Ausgaben entfernt werden, um den Speicher und die Ausführungsgeschwindigkeit des Programms nicht unnötig zu belasten.

## Siehe Auch

- [Arduino Referenz](https://www.arduino.cc/reference/de/): Eine umfassende Sammlung von Befehlen und Funktionen für Arduino
- [Debugging Arduino Code](https://www.arduino.cc/en/Guide/ArduinoDebugging): Offizielle Anleitung zum Debuggen von Arduino-Code
- [Serial.print() vs Serial.println()](https://www.maketecheasier.com/arduino-serial-print-vs-println/): Ein Vergleich der beiden Funktionen und ihre Verwendung im Code