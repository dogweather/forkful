---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-Separated Values", ein Dateiformat, das Daten in tabellarischer Form speichert. Programmierer nutzen CSV häufig für Datenspeicherung und -austausch, weil es einfach, menschenlesbar und weit verbreitet ist.

## How to:
Mit dem Arduino kannst du CSV-Dateien lesen und schreiben. Hier ein Beispiel zum Schreiben von Daten in eine CSV-Datei auf einer SD-Karte:

```Arduino
#include <SD.h>
File dataFile;

void setup() {
  Serial.begin(9600);
  // Stelle sicher, dass deine SD-Karte initialisiert ist
  if (!SD.begin(4)) {
    Serial.println("SD-Kartenfehler");
    return;
  }
  
  dataFile = SD.open("datalog.csv", FILE_WRITE);
  
  // Wenn die Datei geöffnet ist, schreibe Daten:
  if (dataFile) {
    dataFile.println("Temperatur,Luftfeuchtigkeit");
    dataFile.println("23,45");
    dataFile.println("25,50");
    dataFile.close();
    Serial.println("Daten geschrieben");
  } else {
    Serial.println("Fehler beim Öffnen der Datei");
  }
}

void loop() {
  // Nichts zu tun hier
}
```

Sample output:
```
Daten geschrieben
```

## Deep Dive
CSV wurde bereits in den frühen Tagen der Computerei eingeführt, um Daten zwischen unterschiedlichen Anwendungen auszutauschen. Alternativen zu CSV können Formate wie JSON oder XML sein, die strukturiertere Daten ermöglichen, aber auch komplexer sind. Beim Umgang mit CSV auf Arduino muss auf Speicherplatz geachtet werden, da die Board-Ressourcen begrenzt sind. Verarbeiten kannst du CSV-Daten mit String-Splitting oder speziellen Bibliotheken.

## See Also
- Arduino SD-Library Dokumentation: https://www.arduino.cc/en/Reference/SD
- CSV Parsing Bibliothek für Arduino: https://github.com/szymonh/arduino-csv-lib
- Überblick über das Dateiformat CSV: https://tools.ietf.org/html/rfc4180
