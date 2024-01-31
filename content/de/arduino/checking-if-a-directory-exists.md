---
title:                "Überprüfen, ob ein Verzeichnis existiert"
date:                  2024-01-19
html_title:           "Arduino: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist der Prozess, zu verifizieren, dass ein bestimmter Speicherort auf dem Dateisystem vorhanden ist. Programmierer machen das, um Fehler zu vermeiden, die auftreten, wenn auf ein nicht existierendes Verzeichnis zugegriffen wird.

## So geht's:
Verwenden Sie die SD-Bibliothek, um auf das Dateisystem zuzugreifen. Hier ist ein Beispiel:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin()) {
    Serial.println("SD-Kartenfehler");
    return;
  }
  
  File root = SD.open("/");

  if (SD.exists("/meinVerzeichnis")) {
    Serial.println("Verzeichnis existiert!");
  } else {
    Serial.println("Verzeichnis existiert nicht.");
  }
}

void loop() {
  // Nichts zu tun hier
}
```
Beispiel-Ausgabe:
```
Verzeichnis existiert!
```
oder
```
Verzeichnis existiert nicht.
```

## Deep Dive
Die Überprüfung, ob ein Verzeichnis existiert, ist vor allem seit der Einführung von SD- und Mikro-SD-Kartenmodulen für Arduino-Boards wichtig. Früher, wo der Speicher direkt auf dem Mikrocontroller war, gab es weniger dynamischen Speicher. Mit SD-Karten können Verzeichnisse erstellt und gelöscht werden, deshalb muss deren Existenz überprüft werden.

Alternativen gibt es nicht viele – meistens geht es darum, die SD-Bibliothek zu benutzen oder eigene Implementierung für das Dateisystem zu schreiben. Bei der SD-Bibliothek nutzt man `exists()`, um zu prüfen, ob Dateien oder Verzeichnisse vorhanden sind.

Die Implementierungsdetails hangen weitgehend davon ab, wie das Dateisystem auf der SD-Karte strukturiert ist, die File Allocation Table (FAT) ist hier üblich. Die Funktion `exists()` sucht nach dem Eintrag im FAT, um die Existenz zu bestätigen oder zu verneinen.

## Siehe auch
- Die Arduino SD Bibliotheksdokumentation: [https://www.arduino.cc/en/reference/SD](https://www.arduino.cc/en/reference/SD)
- Ein Tutorial zur Dateiverwaltung auf SD-Karten: [https://www.arduino.cc/en/Tutorial/LibraryExamples/Files](https://www.arduino.cc/en/Tutorial/LibraryExamples/Files)
