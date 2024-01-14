---
title:    "Arduino: Debug-Ausgabe drucken"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

##Warum

Es gibt viele Gründe, warum man beim Programmieren von Arduino-Code Debug-Ausgaben drucken möchte. Einer der Hauptgründe ist, dass es eine effektive Möglichkeit ist, um den aktuellen Status des Programms zu überwachen und mögliche Fehler oder Probleme zu identifizieren. Durch das Ausgeben von Debug-Informationen kann man sicherstellen, dass das Programm wie erwartet funktioniert und mögliche Probleme schnell beheben.

##Wie man Debug-Ausgaben druckt

Um Debug-Ausgaben in Arduino zu drucken, gibt es einige grundlegende Schritte, die man befolgen muss. Zunächst muss man die serielle Kommunikation aktivieren, indem man die Funktion `Serial.begin()` in der `setup()` Funktion aufruft. Diese Funktion legt die Baudrate für die serielle Kommunikation fest.

```Arduino
void setup() {
 Serial.begin(9600); // Baudrate von 9600 festlegen
}
```

Danach kann man Debug-Ausgaben mit der Funktion `Serial.println()` drucken. Diese Funktion gibt den angegebenen Wert als Zeichenkette über die serielle Schnittstelle aus und fügt am Ende einen Zeilenumbruch hinzu.

```Arduino
void loop() {
 int sensorValue = analogRead(A0); // Sensorwert auslesen
 Serial.println(sensorValue); // Sensorwert ausgeben
}
```

Auf dem Monitor in der Arduino IDE erscheinen dann die gedruckten Debug-Ausgaben.

##Tiefere Einblicke

Es gibt noch einige weitere nützliche Dinge, die man beim Drucken von Debug-Ausgaben beachten sollte. Zum Beispiel kann man die Funktion `Serial.print()` verwenden, um den Wert ohne Zeilenumbruch auszugeben. Man kann auch verschiedene Datentypen über die serielle Schnittstelle ausgeben, wie zum Beispiel Integer, Floats oder Zeichenketten.

Eine weitere Möglichkeit ist es, die serielle Kommunikation für die Übertragung von Daten zwischen Arduino und einem Computer zu nutzen. In solch einem Fall können Debug-Ausgaben verwendet werden, um die empfangenen Daten auf dem Monitor auszugeben und so die Verbindung zu überprüfen.

##Siehe auch

- [Arduino-Referenz für Serial](https://www.arduino.cc/reference/de/language/functions/communication/serial/)
- [Tutorial: Debugging mit Arduino](https://www.elektronik-kompendium.de/sites/micro/1907101.htm) (auf Deutsch)
- [Serial.println() vs. Serial.print()](https://www.arduino.cc/en/Serial/Print) (auf Englisch)