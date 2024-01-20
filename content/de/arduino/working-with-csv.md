---
title:                "Arbeiten mit CSV"
html_title:           "Arduino: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Was ist CSV & Warum nutzen Programmierer es?
CSV steht für "Comma Separated Values" und ist ein Dateiformat, das verwendet wird, um Daten in Tabellenform darzustellen. Programmierer nutzen CSV, um Daten zwischen verschiedenen Systemen auszutauschen oder um sie in Datenbanken zu importieren und exportieren.

## Wie funktioniert es:
Mit Arduino kannst du Daten in CSV-Format lesen und schreiben. Hier ist ein Beispiel, wie du eine CSV-Datei auf einer SD-Karte erstellen und auslesen kannst:

```Arduino
#include <SPI.h>
#include <SD.h>
#include <String.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ;
  }

  if (!SD.begin(4)) {
    Serial.println("Karte konnte nicht initialisiert werden.");
    return;
  }

  File dataFile = SD.open("daten.csv", FILE_WRITE);

  if (dataFile) {
    dataFile.println("Temperatur, Feuchtigkeit"); //Kopfzeile
    dataFile.println("25, 50"); //Datenzeile
    dataFile.println("26, 55"); //Datenzeile
    dataFile.close();
    Serial.println("Daten erfolgreich geschrieben.");
  }
  else {
    Serial.println("Fehler beim Schreiben der Daten.");
  }
  
  dataFile = SD.open("daten.csv");

  if (dataFile) {
    while (dataFile.available()) {
      Serial.read();
    }
    dataFile.close();
  }
  else {
    Serial.println("Fehler beim Lesen der Daten.");
  }
}

void loop() {
}
```

Die Ausgabe auf dem seriellen Monitor sieht so aus:

```
Daten erfolgreich geschrieben.
Temperatur, Feuchtigkeit
25, 50
26, 55
```

## Tiefere Einblicke:
CSV wurde in den 1970er Jahren entwickelt und ist immer noch ein beliebtes Format für den Datenaustausch. Alternativen wie JSON oder XML werden häufig in modernen Programmiersprachen verwendet, aber CSV hat den Vorteil, dass es einfach zu lesen und zu schreiben ist. Um mit CSV zu arbeiten, musst du die Daten in Zeilen und Spalten aufteilen und trennen.

## Weitere Informationen:
- [Offizielle Arduino Dokumentation zu SD-Karten](https://www.arduino.cc/en/Reference/SD)
- [YouTube Tutorial zu CSV mit Arduino](https://www.youtube.com/watch?v=Sx_kUzXpM18&ab_channel=GreatScoots)