---
title:    "Arduino: Extrahieren von Teilzeichenketten"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilzeichenketten kann für verschiedene Anwendungen in der Arduino-Programmierung nützlich sein. Zum Beispiel kann es hilfreich sein, wenn Sie bestimmte Zeichen aus einer größeren Zeichenfolge entfernen oder bestimmte Teile einer Zeichenfolge für die Verarbeitung isolieren müssen.

## Wie man es macht

Das Extrahieren von Teilzeichenketten in Arduino kann mit der Funktion ```substring()``` durchgeführt werden. Hier ist ein Beispielcode:

```Arduino
String zeichenfolge = "Hallo Welt";
String teilzeichenfolge = zeichenfolge.substring(6);
Serial.println(teilzeichenfolge);
```

Dieser Code würde die Teilzeichenfolge "Welt" aus der ursprünglichen Zeichenfolge "Hallo Welt" extrahieren und auf dem Seriellen Monitor ausgeben.

## Tiefer Einblick

Die Funktion ```substring()``` kann auch mit zwei Parametern verwendet werden, die den Start- und Endindex der Teilzeichenfolge angeben. Zum Beispiel:

```Arduino
String zeichenfolge = "12345";
String teilzeichenfolge = zeichenfolge.substring(1, 4);
Serial.println(teilzeichenfolge);
```

Dieser Code würde die Teilzeichenfolge "234" aus der ursprünglichen Zeichenfolge extrahieren.

Eine wichtige Sache, die beim Extrahieren von Teilzeichenketten zu beachten ist, ist, dass der Endindex nicht in die Teilzeichenfolge eingeschlossen ist. Im obigen Beispiel wurde die Teilzeichenfolge ab dem zweiten Zeichen bis zum vierten Zeichen extrahiert, aber nicht das vierte Zeichen selbst.

## Siehe auch

Weitere Informationen über die Verwendung von ```substring()``` und andere nützliche String-Funktionen in Arduino finden Sie in der Online-Dokumentation:

- [Arduino String Referenz](https://www.arduino.cc/reference/en/language/variables/data-types/string/substring/)
- [Arduino String Funktionen](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)