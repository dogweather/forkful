---
title:                "Arduino: Debug-Ausgabe drucken"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Die Ausgabe von Debug-Informationen ist ein wichtiger Aspekt beim Programmieren mit Arduino. Durch das Drucken von Debug-Output können wir sehen, was im Hintergrund unseres Programms passiert, was uns bei der Fehlersuche helfen kann. Es ist auch eine gute Möglichkeit, den Programmablauf zu überprüfen und zu verstehen.

## Wie

Um Debug-Informationen mit Arduino zu drucken, können wir die Funktion "Serial.print()" verwenden. Diese Funktion gibt den übergebenen Wert auf dem Seriellen Monitor aus. Hier ist ein Beispiel:

```Arduino
int pin = 5;
Serial.print("Pin Nummer:");
Serial.println(pin);
```

Dieser Code würde "Pin Nummer: 5" auf dem Seriellen Monitor ausgeben.

Eine weitere Möglichkeit ist die Verwendung von "Serial.println()", die im Gegensatz zu "Serial.print()" einen Zeilenumbruch am Ende der Ausgabe hinzufügt. Diese Funktion ist nützlich, wenn wir mehrere Werte hintereinander ausgeben möchten. Hier ist ein Beispiel:

```Arduino
int zahl1 = 10;
int zahl2 = 20;

Serial.println("Erste Zahl: ");
Serial.println(zahl1);
Serial.println("Zweite Zahl: ");
Serial.println(zahl2);
```

Die Ausgabe auf dem Seriellen Monitor würde folgendermaßen aussehen:

Erste Zahl:
10
Zweite Zahl:
20

## Deep Dive

Es gibt verschiedene Formatierungsoptionen für die Verwendung von "Serial.print()" und "Serial.println()". Wir können zum Beispiel auch Variablen mit Text kombinieren, indem wir das Pluszeichen verwenden. Hier ist ein Beispiel:

```Arduino
int temp = 25;
Serial.println("Die aktuelle Temperatur beträgt: " + String(temp) + " Grad Celsius");
```

Die Ausgabe würde "Die aktuelle Temperatur beträgt: 25 Grad Celsius" sein.

Außerdem können wir den Datentyp der Ausgabe mit dem Befehl "Serial.println()" ändern, indem wir zusätzliche Parameter hinzufügen. Zum Beispiel, um eine Variable als binären Wert auszugeben, können wir Folgendes verwenden:

```Arduino
int zahl = 47;
Serial.println(zahl, BIN);
```

Die Ausgabe würde "101111" sein, was den binären Wert von 47 darstellt.

## Siehe auch

- [Serial Monitor Basics](https://www.arduino.cc/en/Tutorial/SerialBasics)
- [Debugging Arduino Code](https://www.arduino.cc/en/Tutorial/Debugging)
- [Serial.print() Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Serial.println() Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)