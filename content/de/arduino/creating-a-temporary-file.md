---
title:                "Erstellen einer temporären Datei"
html_title:           "Arduino: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Die Erstellung von temporären Dateien kann in vielen Situationen nützlich sein. Zum Beispiel kann es erforderlich sein, Daten temporär zu speichern, während ein größeres Projekt ausgeführt wird, oder es kann eine Möglichkeit bieten, auf kurzfristige Daten zuzugreifen, ohne bleibende Spuren zu hinterlassen.

## Wie geht das?

Die Verwendung von temporären Dateien in Arduino ist relativ einfach. Zunächst muss die Standardbibliothek für Dateiverwaltung mit `#include <SD.h>` importiert werden. Dann kann eine temporäre Datei mit den folgenden Schritten erstellt werden:

- Ein Objekt der Klasse `SDFile` erstellen und einer vorhandenen Datei zuweisen:
`SDFile tempFile = SD.open("temp.log", FILE_WRITE);`

- Daten in die temporäre Datei schreiben:
```Arduino
tempFile.println("Messwerte:");
tempFile.println(value);
```

- Die Datei schließen, wenn sie nicht mehr benötigt wird:
`tempFile.close();`

Die erstellte temporäre Datei kann wie jede andere Datei im SD-Kartenlaufwerk behandelt werden.

## Tiefere Einblicke

Es ist wichtig zu beachten, dass temporäre Dateien nicht automatisch gelöscht werden, sobald der Arduino ausgeschaltet wird. Um sie zu löschen, muss die Datei manuell geöffnet und dann gelöscht werden. Zum Beispiel:
```Arduino
SD.remove("temp.log");
```

Es gibt auch Möglichkeiten, eine temporäre Datei zu erstellen, ohne sie direkt auf der SD-Karte zu speichern. Dies kann durch die Verwendung von dynamischem Speicher erreicht werden, um einen Zwischenspeicher zu erstellen, in den die Daten geschrieben werden. Dieser Speicher wird dann verwendet, um die Daten später auf die SD-Karte zu schreiben.

## Siehe auch

- [Offizielle Arduino SD library Documentation](https://www.arduino.cc/en/Reference/SD)
- [Tutorial: How to Use SD Card with Arduino](https://www.circuitspecialists.com/blog/using-an-sd-card-with-the-arduino/)