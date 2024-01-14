---
title:    "Arduino: Das Schreiben einer Textdatei"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man ein Textdokument in einem Arduino Programm erstellen würde. Zum Beispiel könnte man Nutzerdaten speichern oder eine Log-Datei für Fehlermeldungen erstellen. Textdateien sind auch hilfreich, um Daten zwischen verschiedenen Programmen auszutauschen.

## Wie erstelle ich eine Textdatei in Arduino

Um eine Textdatei in Arduino zu erstellen, müssen Sie zuerst die Standardbibliothek "SD" importieren. Dann müssen Sie eine SD-Karte in Ihren Arduino einsetzen und zuweisen. Anschließend können Sie einfach den Befehl "open" verwenden, um eine neue Textdatei zu erstellen und darauf zuzugreifen. Hier ist ein Beispiel:

```Arduino
#include <SD.h> // Standardbibliothek importieren

File dataFile; // Textdatei-Variable erstellen

void setup() {
  Serial.begin(9600); // Serielle Kommunikation starten
  SD.begin(4); // SD-Karte zuweisen
  dataFile = SD.open("daten.txt", FILE_WRITE); // Neue Textdatei mit Namen "daten.txt" erstellen
  if (dataFile) { // Überprüfung, ob die Datei erfolgreich geöffnet wurde
    Serial.println("Textdatei erstellt!");
  }
  else { // Wenn die Datei nicht geöffnet werden konnte
    Serial.println("Fehler beim Erstellen der Textdatei!");
  }
}

void loop() {
  // Hier können Sie Ihren Code schreiben, um Daten in die Textdatei zu schreiben
}

```

Wenn Sie das Programm ausführen, wird eine Textdatei mit dem Namen "daten.txt" erstellt und eine Meldung wird ausgegeben, wenn dies erfolgreich war.

## Tiefergehende Informationen

Es gibt verschiedene andere Funktionen und Möglichkeiten, die Sie beim Erstellen einer Textdatei in Arduino nutzen können. Zum Beispiel können Sie mit dem Befehl "print" Daten in die Datei schreiben oder mit dem Befehl "seek" an eine bestimmte Position in der Datei springen. Außerdem können Sie mit dem Befehl "close" die Datei schließen, wenn Sie damit fertig sind. Es gibt auch andere Bibliotheken, die Ihnen helfen können, komplexere Textdateien zu erstellen, wie die "SPIFFS" Bibliothek für Flash-Speicher.

## Siehe auch

- [Arduino SD Library Dokumentation](https://www.arduino.cc/en/reference/SD)
- [Tutorial zur Verwendung von SD-Karten mit Arduino](https://create.arduino.cc/projecthub/electropeak/how-to-use-sd-card-with-arduino-uno-10a2ae)
- [Arduino SPIFFS Library Dokumentation](https://www.arduino.cc/en/Reference/spiffs)