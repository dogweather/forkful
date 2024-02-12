---
title:                "Erstellung einer temporären Datei"
aliases: - /de/arduino/creating-a-temporary-file.md
date:                  2024-01-20T17:39:38.412972-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Arduino Programmierung: Temporäre Dateien nutzen

## Was & Warum?
Temporäre Dateien dienen zum vorübergehenden Speichern von Daten. Sie werden verwendet, wenn man Daten während der Ausführung eines Programms kurzzeitig behalten, aber nicht dauerhaft speichern möchte.

## Anleitung:
Mit Arduino kannst du keine klassischen temporären Dateien wie auf einem Betriebssystem erstellen. Stattdessen verwenden wir den EEPROM, um temporäre Werte zu speichern, oder die SD-Karte für größere Daten.

```Arduino
#include <EEPROM.h>

void setup() {
  Serial.begin(9600);
  // Einen temporären Wert im EEPROM speichern
  EEPROM.write(0, 123); // Die Adresse 0 benutzen, Wert ist 123
}

void loop() {
  // Den gespeicherten Wert lesen
  int tempValue = EEPROM.read(0);
  Serial.print("Temporärer Wert: ");
  Serial.println(tempValue);
  
  // Weitere Logik...
}
```
Ausgabe auf dem Serial Monitor: 
```
Temporärer Wert: 123
```

## Hintergrundwissen:
Zurück in den Tagen vor Microcontrollern, wurden temporäre Dateien auf der Festplatte eines Computers gespeichert. Bei Mikrocontrollern wie dem Arduino haben wir begrenzten Platz und keine traditionelle Festplatte. EEPROM ist eine Möglichkeit, temporäre Daten zu speichern, allerdings ist er nur für eine begrenzte Anzahl von Schreib-/Lesevorgängen ausgelegt. Alternativ kann eine SD-Karte in Kombination mit der `SD` Bibliothek für größere oder häufig veränderliche Daten verwendet werden.

Die Auswahl, ob EEPROM oder SD-Karte kommt auf die Anwendung an. EEPROM ist schneller und einfacher für kleine Datenmengen, während die SD-Karte mit Dateisystemen arbeitet und sich für größere Daten eignet.

## Siehe Auch:
- [EEPROM Library Documentation](https://www.arduino.cc/en/Reference/EEPROM)
- [Arduino - SD Card Tutorial](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)

Auch die Dokumentation zur `SD` Bibliothek kann hilfreich sein, um mit der SD-Karte als Medium für temporäre Dateien zu arbeiten.
