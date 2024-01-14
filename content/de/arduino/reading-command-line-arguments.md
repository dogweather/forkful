---
title:                "Arduino: Lesen von Befehlszeilenargumenten"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Kommandozeilenargumenten ist eine wichtige Fähigkeit, die es ermöglicht, externe Eingaben in ein Arduino-Programm zu integrieren. Dies kann nützlich sein, um Parameter wie Geschwindigkeit oder Konfigurationen anzupassen, ohne den eigentlichen Code ändern zu müssen.

## Wie funktioniert es

Um Kommandozeilenargumente in einem Arduino-Programm zu lesen, gibt es mehrere Schritte. Zunächst müssen die Argumente beim Programmstart übergeben werden. Dies kann in der Arduino IDE unter "Tools" > "Serial Monitor" erfolgen. Anschließend müssen die Argumente im Programm mit dem Befehl "Serial.read()" eingelesen werden. Im unteren Beispiel wird der übergebene Wert ausgelesen und mittels "Serial.print()" ausgegeben.

```Arduino
int arg = Serial.read(); // Argument einlesen
Serial.print("Das übergebene Argument ist: ");
Serial.print(arg); // Argument ausgeben
```

Die Ausgabe im Serial Monitor würde dann folgendermaßen aussehen:

```Arduino
Das übergebene Argument ist: 5
```

## Tiefergehende Information

Neben dem Lesen von einfachen Argumenten können auch Strings und mehrere Argumente gleichzeitig eingelesen werden. Dafür müssen verschiedene Techniken wie die Verwendung von Arrays oder das Parsen von Strings angewendet werden. Es gibt auch verschiedene Libraries, die das Lesen und Verarbeiten von Kommandozeilenargumenten erleichtern.

## Siehe auch

Für weitere Informationen und Beispiele zum Lesen von Kommandozeilenargumenten empfehlen wir folgende Links:

- [Arduino Dokumentation zum Lesen von Seriellen Daten](https://www.arduino.cc/reference/de/language/functions/communication/serial/read/)
- [Tutorial zum Lesen von Kommandozeilenargumenten von Erik Bartmann](https://www.arduino-tutorial.de/arduino-tutorial-30-kommandozeilenparameter)
- [Library "cmdparser" für die Verarbeitung von Kommandozeilenargumenten](https://github.com/majenkotech/cmdparser)