---
title:                "Ein String interpolieren"
html_title:           "Arduino: Ein String interpolieren"
simple_title:         "Ein String interpolieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Interpolieren einer Zeichenfolge ist das Einsetzen von variablen Werten in einen String. Programmierer verwenden dies, um dynamische Inhalte in einem Text anzuzeigen, wie z.B. die Temperatur eines Sensors oder die Uhrzeit.

## Wie geht's?
Um eine Zeichenfolge zu interpolieren, muss zuerst eine Vorlage erstellt werden, in der die zu ersetzenden Variablen mit dem Platzhalter ```%s``` gekennzeichnet werden. Dann wird die Methode ```String::format()``` verwendet, um den String mit den entsprechenden Werten zu füllen.

Beispiel:
```
int sensorValue = 25;
String message = "Die aktuelle Temperatur beträgt %s Grad Celsius.";
String interpolatedString = message.format(sensorValue);

// interpolatedString enthält nun: "Die aktuelle Temperatur beträgt 25 Grad Celsius."
```

## Tiefer Einblick
Das Konzept des String-Interpolierens stammt aus der Programmiersprache Ruby und ist mittlerweile in vielen Sprachen, einschließlich Arduino, verfügbar. Eine alternative Möglichkeit, Variablen in einen String einzufügen, ist die Verwendung der Methode ```String::concat()```, jedoch ist dies weniger platzsparend und effizient.

Bei der Implementierung des String-Interpolierens ist zu beachten, dass die zu ersetzenden Variablen den richtigen Datentyp haben müssen und dass der Platzhalter ```%s``` für Strings verwendet werden muss.

## Siehe auch
Weitere Informationen und Beispiele zum String-Interpolieren finden Sie in der offiziellen Arduino-Dokumentation unter: https://www.arduino.cc/reference/en/language/variables/data-types/stringfunctions/format/