---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt zu starten bedeutet, eine frische, unberührte Programmierumgebung zu erstellen. Programmierer tun dies, um neue Ideen zu verwirklichen, Lösungen für bestehende Probleme zu entwickeln oder ihre Fähigkeiten zu erweitern.

## Anleitung:

Hier ist ein einfaches Beispiel, wie man eine LED auf einem Arduino Uno blinken lässt.

```Arduino
// definiere LED_BUILTIN, wenn sie nicht bereits definiert ist
#ifndef LED_BUILTIN
#define LED_BUILTIN 13
#endif

void setup()
{
  // definiere den digitalen Pin als Ausgabe
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop()
{
  digitalWrite(LED_BUILTIN, HIGH);   // schalte die LED ein
  delay(1000);                       // warte eine Sekunde
  digitalWrite(LED_BUILTIN, LOW);    // schalte die LED aus
  delay(1000);                       // warte eine Sekunde
}
```

Ausgabe:

```Arduino-Ausgabe
LED blinkt alle zwei Sekunden.
```

## Vertiefung:

Arduino basiert auf Wiring, einer Open-Source-Programmiersprache und -Umgebung, die 2003 entwickelt wurde. Als Alternative könnten Sie mikrocontrollerbasierte Plattformen wie Raspberry Pi oder Micro:bit in Betracht ziehen. Der obige Code implementiert den sogenannten Blink-Sketch, der das „Hello, World!“ Äquivalent für eingebettete Systeme ist. Der Loop, der hier implementiert wurde, ermöglicht es der LED, in einem unendlichen Zyklus ein- und auszuschalten.

## Siehe Auch:

1. [Arduino - Home](https://www.arduino.cc/)
2. [Arduino - Getting Started](https://www.arduino.cc/en/Guide/HomePage)
3. [Arduino - Blink](https://www.arduino.cc/en/Tutorial/Blink)