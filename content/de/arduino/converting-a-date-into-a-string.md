---
title:                "Umwandeln eines Datums in eine Zeichenkette"
html_title:           "Arduino: Umwandeln eines Datums in eine Zeichenkette"
simple_title:         "Umwandeln eines Datums in eine Zeichenkette"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Umwandlung eines Datums in einen String bedeutet, dass ein Programmierdatum in ein lesbare Textform verwandelt wird. Programmierer nutzen dies, um die Anzeige von Datum- und Zeitinformationen zu vereinfachen, da es einfacher für Menschen ist, Text als Zahlen zu lesen.

## Anleitung:
### Beispiel 1: Datum in Text konvertieren
```
Arduino String myDate = "16.06.2021";
```
### Beispiel 2: Text in ein Datum konvertieren
```
Arduino int myDate = millis();
```

## Tief in die Materie:
### Historischer Hintergrund:
Schon seit den Anfängen der Programmierung ist die Umwandlung von Daten und Zeit in Text von großer Bedeutung. Frühere Programmiersprachen hatten jedoch nicht immer die integrierte Funktion zur Konvertierung, wie wir sie heute in der Arduino-Sprache haben.

### Alternativen:
Neben der Umwandlung von Datumsangaben in Text gibt es auch andere Möglichkeiten, um diese zu verarbeiten, wie z.B. die Verwendung von numerischen Werten oder die Nutzung von speziellen Bibliotheken, die speziell für die Handhabung von Datum und Zeit entwickelt wurden.

### Implementierungsdetails:
In der Arduino-Sprache wird die Funktion "String" verwendet, um Variablen in Text umzuwandeln. Diese kann sowohl für Datum als auch für andere Daten verwendet werden. Es ist wichtig, die richtigen Formatierungen zu verwenden, um sicherzustellen, dass das Datum korrekt angezeigt wird.

## Siehe auch:
- Arduino offizielle Dokumentation zu Daten und Zeit: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Ein Wörterbuch über Arduino-Befehle: https://www.arduino.cc/reference/de/
- Ein YouTube-Tutorial zur Arduino-Programmierung: https://www.youtube.com/watch?v=nzjCkgX5uff