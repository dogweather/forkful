---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen, die einem Muster entsprechen, ist ein üblicher Vorgang in Arduino-Programmen. Dies ist nützlich, um unerwünschte Zeichen aus Strings zu entfernen und die Daten sauber und konsistent zu halten.

## So geht's:

Mit dem folgenden Code entfernen wir alle Zahlen aus einem String. 

```Arduino
String string1 = "Hallo123Welt456";
for (int i=0; i<=9; i++) {
   string1.remove(string1.indexOf(String(i)), 1);
}
Serial.println(string1);
```
Das Ergebnis dieses Codes wäre `HalloWelt`.

## Tiefer eintauchen:

Das Musterlöschverfahren in der Arduino-Welt hat einen langen Weg zurückgelegt. Es begann mit sperrigen Funktionen und hat sich zu den jetzt üblichen, eingebauten Methoden wie `indexOf` und `remove` entwickelt.

Es gibt auch andere Weisen, Zeichen, die einem Muster entsprechen, zu löschen. Eine Alternative wäre die Nutzung der Funktion `replace`. Allerdings kann diese Methode unpraktisch sein, wenn es viele unterschiedliche Muster gibt.

Alle Musterlöschmethoden verwenden normalerweise eine Art Schleife. Die `indexOf`- und `remove`-Methoden in unserem Beispiel nutzen eine `for`-Schleife, um alle Ziffern von 0 bis 9 zu durchlaufen und zu entfernen.

## Siehe auch:

Für weitere Informationen über Arduino und das Arbeiten mit Zeichenketten in Arduino, besuchen Sie diese Seiten:

 - Die Arduino String API Dokumentation: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
 - Eine gründliche Erläuterung zum Arbeiten mit Zeichenketten: https://learn.adafruit.com/multi-tasking-the-arduino-part-3/dealing-with-strings
 - Ein Tutorial zum Erkennen und Löschen von Zeichen: https://startingelectronics.org/software/arduino/remove-characters-from-string/