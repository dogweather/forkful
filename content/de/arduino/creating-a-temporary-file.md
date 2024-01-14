---
title:                "Arduino: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man temporäre Dateien in einem Arduino-Programm erstellen möchte. Zum Beispiel kann es sein, dass man Daten zwischen verschiedenen Funktionen oder Schleifen speichern muss, oder dass man bestimmte Dateien nur vorübergehend benötigt. Egal aus welchem Grund, das Erstellen und Nutzen von temporären Dateien kann sehr nützlich sein.

## Wie

Es gibt einige Schritte, die man befolgen muss, um erfolgreich temporäre Dateien in einem Arduino-Programm zu erstellen. Zuerst muss man eine Variable erstellen, die den Speicherort der temporären Datei speichert. Dann muss man Funktionen nutzen, um die temporäre Datei zu erstellen, zu schreiben und zu löschen.

```Arduino
File tempFile; // erstelle eine Variable für die temporäre Datei

void setup() {
  // initialisiere serielle Kommunikation
  Serial.begin(9600);

  // erstelle eine neue temporäre Datei mit dem Namen "temp.txt"
  tempFile = SD.open("temp.txt", FILE_WRITE);

  // überprüfe, ob die Datei erfolgreich erstellt wurde
  if (!tempFile) {
    Serial.println("Fehler beim Erstellen der temporären Datei!");
  }
}

void loop() {
  // schreibe Daten in die temporäre Datei
  tempFile.println("Dies ist ein Beispieltext.");

  // schließe die Datei
  tempFile.close();

  // lösche die temporäre Datei
  SD.remove("temp.txt");

  // warte eine Sekunde
  delay(1000);
}
```

## Deep Dive

Beim Erstellen einer temporären Datei gibt es einige wichtige Dinge zu beachten. Zunächst muss man sicherstellen, dass man ausreichend Speicherplatz auf dem Arduino verfügbar hat, um die temporäre Datei zu erstellen. Außerdem muss man darauf achten, dass man die Datei am Ende wieder löscht, um Speicherplatz frei zu machen.

Eine weitere wichtige Überlegung ist die Benennung der temporären Datei. Es ist wichtig, einen eindeutigen und aussagekräftigen Namen zu wählen, um Verwechslungen mit anderen Dateien zu vermeiden.

## Siehe auch

- [Documentation Arduino - Creating and Deleting Temporary Files](https://www.arduino.cc/en/Tutorial/Files)
- [Tutorialspoint - Arduino SD Library](https://www.tutorialspoint.com/arduino/arduino_sd_library.htm)
- [Instructables - How to Use the Arduino SD Card Library](https://www.instructables.com/How-to-Use-The-Arduino-SD-Card-Library/)