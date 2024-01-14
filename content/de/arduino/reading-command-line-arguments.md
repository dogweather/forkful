---
title:                "Arduino: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Lesen von Befehlszeilenargumenten bei Arduino beschäftigen? Ganz einfach: Es ermöglicht dir, deine Programme noch effizienter und flexibler zu gestalten. Indem du Befehle über die Kommandozeile eingeben kannst, kannst du deine Arduino-Projekte individuell an deine Bedürfnisse anpassen.

## So geht's

Um Befehlszeilenargumente in deinem Arduino-Code zu lesen, musst du zunächst das `Serial`-Objekt initialisieren. Anschließend kannst du den Befehl `Serial.readString()` verwenden, um die Eingabe des Nutzers zu lesen. Hier ein Beispielcode:

```Arduino
void setup() {
    Serial.begin(9600); // Initialisiere das Serial-Objekt
}

void loop() {
    if (Serial.available()) { // Überprüfe, ob ein Befehl eingegeben wurde
        String input = Serial.readString(); // Lese den Befehl ein
        // Hier kannst du nun mit dem Befehl arbeiten und z.B. LEDs steuern 
    }
}
```

Wenn du nun beispielsweise den Befehl `LED ON` über die serielle Schnittstelle eingibst, könntest du mit einem einfachen `if`-Statement und dem Befehl `Serial.println()` die LED einschalten und eine Bestätigung über die serielle Schnittstelle zurückgeben.

## Tiefergehende Informationen

Es gibt zahlreiche Anwendungsmöglichkeiten für das Lesen von Befehlszeilenargumenten in deinen Arduino-Projekten. Du könntest beispielsweise eine Fernbedienung für deine Projekte erstellen, indem du über die serielle Schnittstelle Befehle sendest. Oder du könntest deinen Arduino als interaktive Musikinstrument nutzen, indem du über die Befehlszeile Noten und Töne eingibst.

Eine wichtige Sache, die du beim Lesen von Befehlszeilenargumenten beachten musst, ist die Sicherheit. Stelle sicher, dass deine Befehle überprüft und validiert werden, um unerwünschte oder gefährliche Eingaben zu verhindern.

## Siehe auch

- [Offizielle Arduino Dokumentation](https://www.arduino.cc/reference/de/language/functions/communication/serial/readstring/)
- [Tutorial: Fernbedienung für Arduino-Projekte](https://wiki.makerspace-bremen.de/projekte/fernbksmdung-mit-einem-arduino/)