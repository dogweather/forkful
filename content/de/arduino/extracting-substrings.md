---
title:                "Extrahieren von Teilstrings"
html_title:           "Arduino: Extrahieren von Teilstrings"
simple_title:         "Extrahieren von Teilstrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Manchmal ist es notwendig, aus einem String in Ihrem Arduino-Code einen bestimmten Teil herauszuziehen, um damit zu arbeiten. Dies kann nützlich sein, um beispielsweise bestimmte Informationen aus einer seriellen Nachricht zu erhalten oder Daten aus einem Sensorwert zu extrahieren.

## Wie geht man vor

Um Substrings aus einem String zu extrahieren, gibt es verschiedene Methoden in der Programmiersprache Arduino. Eine davon ist die Verwendung der Funktion `substring()`, die es ermöglicht, einen Teil des ursprünglichen Strings basierend auf der Startposition und der Anzahl der zu extrahierenden Zeichen zu erhalten. Schauen wir uns dazu ein Beispiel an:

```Arduino
String text = "Hallo, ich bin ein Arduino";
String substring = text.substring(7, 16);
Serial.println(substring); // Ausgabe: "ich bin ein"
```

In diesem Beispiel wird der Teil des Strings von der Position 7 bis zur Position 16 in der Variable `substring` gespeichert und dann über `Serial.println()` ausgegeben. Beachten Sie, dass die Positionen in Strings immer bei 0 beginnen.

## Tiefer Einblick

Die `substring()`-Funktion ist hilfreich, aber sie hat auch einige Einschränkungen. Zum Beispiel müssen Sie immer einen festen Startpunkt und eine feste Anzahl von Zeichen angeben, was in manchen Situationen unpraktisch sein kann. Eine weitere Möglichkeit ist die Verwendung von `indexOf()` und `lastIndexOf()`, um die Position von bestimmten Zeichen im String zu finden und dann `substring()` zu verwenden, um den gewünschten Teil zu extrahieren. Für eine detailliertere Erklärung dieser Methoden und weitere Optionen empfehle ich, sich mit der offiziellen Arduino-Dokumentation vertraut zu machen.

## Siehe auch

- Offizielle Arduino-Dokumentation zu Strings: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Detaillierte Erklärung von `substring()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/