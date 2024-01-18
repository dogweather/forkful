---
title:                "Ein Datum aus einem Strings auslesen"
html_title:           "Arduino: Ein Datum aus einem Strings auslesen"
simple_title:         "Ein Datum aus einem Strings auslesen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen des Datums aus einem String ist ein häufiges Verfahren in der Programmierung und bezieht sich auf die Extrahierung eines Datums aus einer Zeichenfolge, die möglicherweise andere Zeichen oder Wörter enthält. Programmierer nutzen diese Technik, um die Daten aus Benutzereingaben oder anderen Quellen zu extrahieren und in einem bestimmten Format zu speichern oder weiter zu verarbeiten.

## Wie Geht's?

Das folgende Beispiel zeigt, wie man das Datum aus einer Zeichenfolge in Arduino extrahieren kann:

```Arduino
String zeichenfolge = "15.05.2021";
int tag = zeichenfolge.substring(0,2).toInt(); //Extrahiert Tag (15) und konvertiert in Integer
int monat = zeichenfolge.substring(3,5).toInt(); //Extrahiert Monat (05) und konvertiert in Integer
int jahr = zeichenfolge.substring(6,10).toInt(); //Extrahiert Jahr (2021) und konvertiert in Integer

Serial.print("Tag: ");
Serial.println(tag); //Gibt 15 aus
Serial.print("Monat: ");
Serial.println(monat); //Gibt 5 aus
Serial.print("Jahr: ");
Serial.println(jahr); //Gibt 2021 aus
```

## Tief Tauchen

Das Parsen von Daten aus einer Zeichenfolge hat eine lange Geschichte und wird in vielen Programmiersprachen verwendet. Alternativ können Programmierer auch reguläre Ausdrücke nutzen, um die Datumsinformationen zu extrahieren. Die genaue Implementierung kann je nach Programmiersprache und gewünschtem Ergebnis variieren.

## Siehe Auch

- [Dokumentation zu String-Methoden in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Reguläre Ausdrücke in der Programmierung](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)