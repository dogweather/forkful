---
title:    "Arduino: Fehlersuche-Ausgabe drucken"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

##Warum

Das Drucken von Debug-Ausgaben ist ein wichtiger Aspekt bei der Arduino-Programmierung. Es ermöglicht Entwicklern, den genauen Ablauf ihres Codes zu verfolgen und mögliche Fehler zu erkennen. Dadurch spart man Zeit und Nerven bei der Fehlerbehebung.

##Wie funktioniert es?

In der Arduino-Programmierumgebung gibt es die Funktion "Serial.print()", mit der man Debug-Ausgaben auf dem seriellen Monitor anzeigen kann. Hier ist ein Beispiel, wie man eine Debug-Ausgabe mit der Nachricht "Hello World" erstellt:

```Arduino
void setup() {
  Serial.begin(9600); //Initialisierung der seriellen Verbindung
}

void loop() {
  Serial.print("Hello World"); //Debug-Ausgabe wird auf dem seriellen Monitor angezeigt
  delay(1000); //Eine Sekunde warten
}
```

Die Ausgabe des obigen Codes würde "Hello World" alle Sekunde auf dem seriellen Monitor anzeigen. Dies ist besonders nützlich, um zu überprüfen, ob bestimmte Teile des Codes ausgeführt werden oder um Variablenwerte zu überwachen.

##Tiefere Einblicke

Neben der grundlegenden "Serial.print()" Funktion gibt es auch weitere Möglichkeiten, Debug-Ausgaben zu formatieren und zu steuern. Hier sind einige Beispiele:

- Verwendung von "Serial.println()", um eine neue Zeile nach jeder Ausgabe hinzuzufügen.
- Verwendung von "Serial.write()", um rohe Daten auszugeben.
- Formatieren von Ausgaben mit Variablen, z.B. "Serial.print("Temperatur: "); Serial.print(temperatur); Serial.println(" °C");"

Eine vollständige Liste aller verfügbaren Funktionen und deren Verwendung finden Sie in der [Arduino-Referenz](https://www.arduino.cc/reference/de/).

##Siehe auch

- [Offizielle Arduino-Website](https://www.arduino.cc/)
- [Arduino-Programmierhandbuch](https://www.arduinohandbuch.de/)
- [Arduino-Forum](https://forum.arduino.cc/)