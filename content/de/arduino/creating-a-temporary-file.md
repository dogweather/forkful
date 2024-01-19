---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Eine temporäre Datei ist eine Datei, die zur vorübergehenden Speicherung von Daten während der Laufzeit eines Programms erstellt wird. Programmierer nutzen sie, um den Speicher effizient zu nutzen, indem sie nicht benötigte Daten vorübergehend auslagern.

## So geht's:

Im Arduino gibt es leider keine spezielle Funktion zum Erstellen temporärer Dateien, da es auf einer Microcontroller-Plattform basiert, die ein einfacheres Dateisystem nutzt. Aber dennoch kann man „temporäre“ Dateien mit der SD-Bibliothek erstellen. Hier ist ein Beispiel:

```Arduino
#include <SD.h>

void setup()
{
  Serial.begin(9600);
  SD.begin(4);

  File tempFile = SD.open("temp.txt", FILE_WRITE);
  
  if (tempFile) {
    tempFile.println("Dies ist eine temporäre Datei!");
    tempFile.close();
    Serial.println("Temporäre Datei erfolgreich erstellt.");
  }
  else {
    Serial.println("Fehler beim Erstellen der temporären Datei!"); 
  }
}

void loop() {
}
```

Das obige Programm erstellt eine Datei namens "temp.txt" und schreibt die Zeichenkette "Dies ist eine temporäre Datei!" hinein. 

## Vertiefung

Da Arduino eine microcontroller-basierte Plattform ist, ist seine Dateioperation im Vergleich zu vollwertigen Betriebssystemen sehr begrenzt. Einer der Gründe ist, dass viele Mikrocontroller, einschließlich derer auf Arduino-Platinen, keinen Zugriff auf ein vollwertiges Dateisystem oder eine Festplatte wie ein Desktop-Computer haben.

Es gibt Alternativen zur Verwendung von temporären Dateien auf Geräten mit begrenzten Ressourcen wie Arduino. Eine Option ist die Nutzung des EEPROMs (electrically erasable programmable read-only memory), obwohl wegen seiner begrenzten Lebensdauer der Schreibzyklen Vorsicht geboten ist.

## Weiterführendes

- Arduino SD Library: https://www.arduino.cc/en/Reference/SD
- EEPROM-Bibliothek: https://www.arduino.cc/en/Reference/EEPROM