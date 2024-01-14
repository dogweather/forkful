---
title:    "Arduino: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Funktion beim Programmieren eines Arduino-Geräts. Es ermöglicht Ihnen, sicherzustellen, dass wichtige Dateien oder Daten vorhanden sind, bevor Sie diese bearbeiten oder darauf zugreifen.

# How To

Das Überprüfen, ob ein Verzeichnis existiert, kann einfach erreicht werden, indem Sie die `File::exists()` Funktion verwenden. Hier ist ein Beispielcode, um ein Verzeichnis mit dem Namen "Daten" zu überprüfen:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  // SD-Karte initialisieren
  if (!SD.begin(4)) {
    Serial.println("SD-Karte konnte nicht initialisiert werden.");
    return;
  }  
}

void loop() {
  if (SD.exists("Daten")) {
    Serial.println("Das Verzeichnis existiert.");
  } else {
    Serial.println("Das Verzeichnis konnte nicht gefunden werden.");
  }
}
```

Wenn das Verzeichnis "Daten" auf der SD-Karte vorhanden ist, wird die Ausgabe "Das Verzeichnis existiert." auf der seriellen Schnittstelle angezeigt. Andernfalls wird die Ausgabe "Das Verzeichnis konnte nicht gefunden werden." gegeben.

# Deep Dive

Zusätzlich zur Überprüfung eines Verzeichnisses können Sie auch überprüfen, ob eine Datei in diesem Verzeichnis vorhanden ist, indem Sie den kompletten Pfad zur Datei angeben, z.B. `Daten/daten.txt`. Sie können auch die `SD.begin()` Funktion verwenden, um auf eine andere SD-Karte oder ein anderes Laufwerk zuzugreifen.

Es ist auch wichtig zu beachten, dass die Verwendung von `SD.exists()` nur überprüft, ob das Verzeichnis oder die Datei existieren. Es überprüft nicht, ob es tatsächlich lesbar oder beschreibbar ist. Um dies zu überprüfen, können Sie die Funktionen `SD.open()` oder `SD.mkdir()` verwenden.

# Siehe auch

- SD-Bibliothek Referenz: https://www.arduino.cc/en/Reference/SD
- Arduino SD Library Tutorial: https://www.arduino.cc/en/Tutorial/LibraryExamples/SD