---
title:                "Arduino: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie in die Welt der Arduino-Programmierung eintauchen, stehen Sie vor einer Vielzahl von Möglichkeiten und Herausforderungen. Eine davon ist die Verarbeitung von CSV-Dateien. CSV steht für "Comma Separated Values" und ist ein beliebtes Dateiformat für den Austausch von Daten. Mit der Verwendung von CSV können Sie Daten in einer tabellenähnlichen Struktur speichern und später wieder aufrufen. Wenn Sie also mit umfangreichen Daten arbeiten und diese in Ihrem Arduino-Projekt verwenden möchten, ist es wichtig zu wissen, wie man mit CSV umgeht.

## Wie man mit CSV arbeitet

Um mit CSV in Ihrem Arduino-Projekt zu arbeiten, müssen Sie zuerst die entsprechende Bibliothek importieren. Dies können Sie ganz einfach über die Arduino-IDE tun, indem Sie auf "Sketch" klicken, dann auf "Include Library" und schließlich auf "Manage Libraries". Suchen Sie nach "CSV" und installieren Sie die Bibliothek. Nun können Sie mit dem Verarbeiten von CSV-Dateien beginnen.

Um eine CSV-Datei zu öffnen und die Daten zu lesen, müssen Sie zuerst eine Variable vom Typ "File" erstellen und sie mit der entsprechenden Datei initialisieren. Dann können Sie mit der Funktion "file.available()" überprüfen, ob die Datei geöffnet ist und Sie auf die Daten zugreifen können. Mit der Funktion "file.readStringUntil('\n')" können Sie dann eine Zeile aus der Datei lesen und die Daten in einer Variablen speichern. Um alle Zeilen in der Datei zu durchlaufen, können Sie dies in einer Schleife tun und jede Zeile einzeln verarbeiten. Ein Beispielcode könnte so aussehen:

```Arduino
#include <CSV.h> // Bibliothek importieren

File myFile; // Variable vom Typ "File"

void setup() {
  Serial.begin(9600); // Serielle Verbindung starten
  myFile = SD.open("data.csv"); // Datei öffnen und in Variable speichern
}

void loop() {
  if (myFile.available()) { // Überprüfen, ob Datei geöffnet ist
    String line = myFile.readStringUntil('\n'); // Eine Zeile aus Datei lesen und in Variable speichern
    Serial.println(line); // Gelesene Zeile auf serielle Schnittstelle ausgeben
  }
  else { // Wenn Datei nicht geöffnet werden konnte
    Serial.println("Error opening file"); // Fehlermeldung ausgeben
  }
}
```

Dieses Beispiel liest alle Zeilen aus der Datei "data.csv" aus und gibt sie auf der seriellen Schnittstelle aus. Je nach Anwendungsfall können Sie die gelesenen Daten in Variablen speichern und weiterverarbeiten.

## Tiefere Einblicke in die Arbeit mit CSV

Es gibt viele Möglichkeiten, wie Sie mit CSV-Dateien in Ihrem Arduino-Projekt arbeiten können. Zum Beispiel können Sie mit der Funktion "file.readStringUntil(',')", die Daten in einer Zeile nach Kommas trennen und jeweils in einer Variablen speichern. Oder Sie können mit der Funktion "file.readBytes()" eine bestimmte Anzahl von Bytes aus der Datei lesen und in einer Variable speichern. Es gibt auch Funktionen zum Schreiben von Daten in eine CSV-Datei, zum Bearbeiten bestehender CSV-Dateien und vieles mehr. Wenn Sie tiefer in die Materie einsteigen möchten, sollten Sie sich die offizielle Dokumentation der CSV-Bibliothek ansehen und die verschiedenen Funktionen und Möglichkeiten erkunden.

## Siehe auch
- [CSV-Bibliothek für Arduino](https://github.com/joshuamir/csv-arduino)
- [Offizielle Arduino-Dokumentation](https://www.arduino.cc/reference/en/libraries/csv/)
- [Beispielprojekt für die Verwendung von CSV auf dem Arduino](https://create.arduino.cc/projecthub/Harsh126/arduino-using-csv-data-from-excel-d98960)