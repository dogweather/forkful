---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:46:39.989710-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Ermitteln der Länge eines Strings bedeutet, zu zählen, wie viele Zeichen er enthält. Programmierer müssen das oft tun, um Speicher zu managen, für Validierungen oder um mit Teilstrings zu arbeiten.

## Anleitung:

Um die Länge eines Strings in Arduino zu finden, verwendest du die `length()`-Methode des String-Objekts. Hier ein schnelles Beispiel:

```arduino
String meinText = "Hallo Welt";
int laenge = meinText.length();

Serial.begin(9600);
Serial.println(laenge); // Gibt "10" aus, da "Hallo Welt" 10 Zeichen hat.
```

Achte darauf, dass `length()` ein `int` zurückliefert - die Anzahl der Zeichen im String.

## Tiefere Einblicke:

Historisch betrachtet, lange bevor die `String`-Klasse in Arduino eingeführt wurde, manipulierten Entwickler Zeichenketten eher auf niedriger Ebene mit C-Strings oder char-Arrays. Die Länge eines C-Strings zu finden, involvierte normalerweise die Funktion `strlen()` aus der Standardbibliothek `string.h`.

Es gibt Alternativen zur `String`-Klasse, wie das Verwenden von Char-Arrays. Hierfür würde die Funktion `strlen()` aus der C-Standardbibliothek zum Einsatz kommen. Beachte, dass `String`-Objekte mehr Speicher beanspruchen können als einfache Char-Arrays, was auf Mikrocontrollern mit begrenztem Speicher kritisch werden könnte. 

Der interne Aufbau der `String`-Klasse in Arduino behandelt dynamische Speicherzuweisungen und kann durch Fragmentierung zu Speicherproblemen führen, vor allem in lang laufenden oder komplexen Programmen. Deshalb ziehen manche Entwickler Char-Arrays vor, da bei diesen die Speicherverwaltung kontrollierbarer ist.

## Siehe auch:

- Arduino Referenz für die `String`-Klasse: [https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- C Standardbibliothek `string.h` für Manipulationen mit C-Strings: [http://www.cplusplus.com/reference/cstring/](http://www.cplusplus.com/reference/cstring/)
- Diskussionen zu Speichernutzung und Management in Arduino-Projekten: [https://forum.arduino.cc/](https://forum.arduino.cc/)
