---
title:                "Das Lesen einer Textdatei"
html_title:           "Arduino: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei ist eine häufige Aufgabe in der Programmierung, bei der es darum geht, den Inhalt einer Textdatei in ein elektronisches Format zu übertragen, das von einem Computer gelesen werden kann. Programmierer verwenden diese Technik, um Daten zu importieren oder zu analysieren oder um Benutzereingaben zu verarbeiten.

## Anleitung:

Um eine Textdatei mit Arduino zu lesen, müssen wir zunächst die Textdatei auf der SD-Karte speichern. Dazu benötigen wir ein SD-Kartenmodul, das an den Arduino angeschlossen wird. Dann können wir die `SD` Library in unserem Code verwenden, um auf die Datei zuzugreifen und den Inhalt zu lesen.

```Arduino 
#include <SD.h> // SD-Library einbinden
File myFile; // Erstellen des Dateiobjekts

void setup() {
  Serial.begin(9600);
  // Initialisierung der SD-Karte
  if (!SD.begin(4)) {
    Serial.println("SD-Karte konnte nicht initialisiert werden!");
    return;
  }
  Serial.println("SD-Karte initialisiert.");
  
  // Öffnen der Textdatei zum Lesen
  myFile = SD.open("text.txt", FILE_READ);

  if (myFile) {
    Serial.println("Textdatei geöffnet:");
    
    // Ausgabe des Dateiinhalts Zeile für Zeile
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    
    myFile.close(); // Schließen der Datei
  } else {
    Serial.println("Fehler beim Öffnen der Datei.");
  }
}

void loop() {
  
}
```

Die Ausgabe im Seriellen Monitor wird so aussehen:

``` 
Textdatei geöffnet:
Hallo, dies ist eine Beispieltextdatei.
Hier steht noch mehr Text.
```

## Tiefere Einblicke:

Die Möglichkeit, Textdateien zu lesen, war historisch gesehen ein wichtiger Entwicklungsschritt bei der Programmierung. Frühere Computer konnten oft nur mit harten Codes arbeiten, was die Bearbeitung von Programmen schwierig machte. Das Lesen von Textdateien ermöglichte es Programmierern, flexiblere und leichter zu bearbeitende Programme zu entwickeln.

Alternativ können auch andere Methoden verwendet werden, um Textdateien mit Arduino zu lesen, z.B. die `Serial` und `SPI` Libraries oder die Nutzung des USB-Host-Schilds. Beim Lesen von Textdateien ist es wichtig, auch auf die Codierung der Datei zu achten, um sicherzustellen, dass der Inhalt korrekt interpretiert wird.

## Siehe auch:

- [Arduino SD Library Dokumentation](https://www.arduino.cc/en/Reference/SD)
- [Tutorial für das Lesen von Textdateien auf der SD-Karte mit Arduino](https://www.developershome.com/read/text/readTextArduino.asp)