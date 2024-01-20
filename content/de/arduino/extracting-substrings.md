---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilstrings (Substrings) ist das Herausnehmen von kleinen Teilen aus einem größeren String. Diese Fähigkeit ist unverzichtbar, wenn du bestimmte Teile Daten aus einem gesamten Informationsstring benötigst.

## So geht's:
In Arduino verwenden wir die Methode substring(). Hier ist ein Beispiel, wie es funktioniert:

```Arduino
String str = "Hallo, Welt!";
String substr = str.substring(7, 12);
Serial.println(substr);  // Ausgabe: "Welt"
```

In diesem Beispiel wird ein Teilstring, der aus den Zeichen 7 bis 11 besteht (beachte, dass das zweite Argument den Index nach dem letzten gewünschten Zeichen angibt) aus dem String "str" extrahiert und als "substr" gespeichert. Die Ausgabe ist 'Welt'.

## Vertiefung
Das Extrahieren von Substrings ist ein klassischer Ansatz, der seit den Anfängen der Programmierung genutzt wird. Es gibt Alternativen zu substring() wie strtok() in der C-Standardbibliothek, aber die Verwendung von substring() in Arduino ist am einfachsten, da es direkt die String-Klasse verwendet und keine zusätzlichen Bibliotheken benötigt.

Die Implementierung von substring() in Arduino erstellt intern eine neue Instanz der String-Klasse und kopiert die Zeichen des Ursprungs-Strings in den neuen String. Es ist wichtig, den Speicherverbrauch im Auge zu behalten, da dies einen Einfluss auf die Performance haben kann.

## Siehe auch
Um dein Wissen über das Arbeiten mit Strings in Arduino weiter zu vertiefen, sieh dir bitte die folgenden Links an:

1. [Arduino String-Referenz](https://www.arduino.cc/reference/de/language/variables/data-types/string/): Hier findest du die offizielle Dokumentation zur Arbeit mit Strings in Arduino.
2. [Substring in Arduino: A complete guide](https://www.makerguides.com/arduino-string-substring/): In diesem Leitfaden werden alle Aspekte bezüglich Teilstrings in Arduino eingehend behandelt.