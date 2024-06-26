---
date: 2024-01-20 17:55:13.479512-07:00
description: "So geht's: Arduino-Plattformen haben normalerweise keine traditionelle\
  \ Kommandozeile, wie man sie von Desktop-Betriebssystemen kennt. Stattdessen kannst\u2026"
lastmod: '2024-03-13T22:44:54.158763-06:00'
model: gpt-4-1106-preview
summary: Arduino-Plattformen haben normalerweise keine traditionelle Kommandozeile,
  wie man sie von Desktop-Betriebssystemen kennt.
title: Lesen von Kommandozeilenargumenten
weight: 23
---

## So geht's:
Arduino-Plattformen haben normalerweise keine traditionelle Kommandozeile, wie man sie von Desktop-Betriebssystemen kennt. Stattdessen kannst du Parameter über die serielle Schnittstelle mit `Serial.read()` oder `Serial.parseInt()` einlesen. Hier ein einfaches Beispiel:

```arduino
void setup() {
  Serial.begin(9600); // Starte die serielle Verbindung
  while (!Serial) {
    ; // Warte auf die serielle Verbindung
  }
  Serial.println("Gib einen Wert ein:");
}

void loop() {
  if (Serial.available() > 0) {
    int receivedValue = Serial.parseInt(); // Lese den nächsten Integerwert
    Serial.print("Empfangener Wert: ");
    Serial.println(receivedValue);
  }
}
```

Ein Test könnte so aussehen:
```
Gib einen Wert ein:
> 42
Empfangener Wert: 42
```

## Tiefgang:
Auf einem typischen PC kannst du Argumente über die Kommandozeile übergeben; auf Mikrocontrollern wie dem Arduino ist dies nicht Standard. Historisch gesehen wurden Parameter in eingebetteten Systemen oft über Dip-Schalter oder Jumper gesetzt. Alternativen zum Lesen von Parametern auf dem Arduino sind unter anderem das Auslesen von Konfigurationsdateien von einer SD-Karte, das Einstellen von Variablen im Code selbst vor dem Hochladen oder die Verwendung von DIP-Schaltern auf der Hardware. Letztere Methode ist allerdings weniger flexibel und erfordert eine physikalische Anwesenheit am Gerät.

## Siehe Auch:
- Arduino Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Arduino und SD-Kartenleser: https://www.arduino.cc/en/reference/SD
- Einführung in Arduino und serielle Kommunikation: https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent
