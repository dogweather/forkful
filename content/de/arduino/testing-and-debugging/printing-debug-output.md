---
title:                "Debug-Ausgaben drucken"
date:                  2024-01-20T17:51:49.348762-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Debug-Ausgaben drucken heißt, Informationen während der Laufzeit eines Programms zur Problemdiagnose auszugeben. Entwickler nutzen es, um den Programmfluss zu verstehen und Fehler schnell zu finden.

## How to:
Ein einfaches Beispiel, um Daten auf der seriellen Schnittstelle auszugeben:

```Arduino
void setup() {
  Serial.begin(9600); // Startet die serielle Kommunikation mit 9600 Baud
}

void loop() {
  Serial.println("Hello, Debug!"); // Druckt eine Nachricht auf der seriellen Konsole
  delay(1000); // Wartet eine Sekunde
}
```

Wenn das Programm läuft, solltest du alle Sekunde "Hello, Debug!" im seriellen Monitor sehen.

## Deep Dive
Das Drucken von Debug-Ausgaben ist so alt wie das Programmieren selbst. Früher wurden Ausgaben auf Papier gedruckt oder an Terminalgeräte gesendet. Heutzutage nutzen wir meist integrierte Entwicklungsumgebungen (IDEs) und serielle Monitore, wie in der Arduino IDE.

Alternativen zum seriellen Debugging sind das Nutzen von LEDs oder LCD-Displays, um den Status des Programms anzuzeigen, besonders nützlich, wenn der serielle Port gerade anderweitig verwendet wird.

Die serielle Ausgabe in Arduino erfolgt über die UART-Schnittstelle (Universal Asynchronous Receiver/Transmitter), welche digitale Pins benutzt (meistens 0 (RX) und 1 (TX)). Es gibt auch erweiterte Bibliotheken wie "SoftwareSerial" für mehr Flexibilität bezüglich der verwendeten Pins.

## See Also
Für weitere Informationen und Debug-Techniken schaue dir bitte die folgenden Links an:

- Arduino Referenz zum Serial: https://www.arduino.cc/reference/de/language/functions/communication/serial/
- SoftwareSerial Bibliothek: https://www.arduino.cc/en/Reference/softwareSerial
- Arduino Debugging Techniques: https://www.arduino.cc/en/Tutorial/LibraryExamples/SoftwareSerialExample
