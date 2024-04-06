---
date: 2024-01-20 17:44:58.321543-07:00
description: "How to: Arduino-Programmierer extrahieren Substrings schon seit den\
  \ fr\xFChen Tagen des Boards, um Sensordaten zu parsen oder Kommunikation zu erleichtern.\u2026"
lastmod: '2024-04-05T21:53:56.016936-06:00'
model: gpt-4-1106-preview
summary: "Arduino-Programmierer extrahieren Substrings schon seit den fr\xFChen Tagen\
  \ des Boards, um Sensordaten zu parsen oder Kommunikation zu erleichtern."
title: Teilstrings extrahieren
weight: 6
---

## How to:
```Arduino
String text = "Hallo Welt, hier Arduino!";
int start = 6;
int ende = 10;
String teilString = text.substring(start, ende);

Serial.begin(9600);
while(!Serial); // Warten auf die Serial-Verbindung
Serial.println(teilString); // Gibt "Welt" aus
```
Ausgabe:
```
Welt
```

## Deep Dive
Arduino-Programmierer extrahieren Substrings schon seit den frühen Tagen des Boards, um Sensordaten zu parsen oder Kommunikation zu erleichtern. Während `substring()` die gängige Methode in der Arduino-Welt ist, nutzen andere Sprachen Methoden wie `substr()` oder `slice()`. Bei der Implementierung ist zu beachten, dass der start-Index inklusiv, der ende-Index jedoch exklusiv ist. Für ressourcenbeschränkte Systeme ist es wichtig, Effizienz und Speichernutzung bei der Verwendung von String-Operationen zu berücksichtigen.

## See Also
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/substring/
- Arduino Forum: https://forum.arduino.cc/
- Tutorial zum Arbeiten mit Strings in Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringSubstring
