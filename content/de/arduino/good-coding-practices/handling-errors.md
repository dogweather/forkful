---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:24:08.954582-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Fehlerbehandlung in Ihren Programmen fängt die unvorhergesehenen Dinge auf, die versuchen, Ihnen ein Bein zu stellen. Sie tun dies, um zu verhindern, dass Ihr Arduino abschmiert, wenn das Unerwartete passiert.

## Wie geht das:

Nehmen wir an, Ihr Arduino liest einen Sensor, der gelegentlich Werte außerhalb des Bereichs produzieren kann. So könnten Sie das handhaben:

```Arduino
int sensorWert = analogRead(A0);

if (sensorWert >= 0 && sensorWert <= 1023) {
  // Wert liegt im Bereich, weiter mit der Verarbeitung
  Serial.println(sensorWert);
} else {
  // Wert liegt außerhalb des Bereichs, Fehler behandeln
  Serial.println("Fehler: Sensorwert außerhalb des Bereichs.");
}
```
Beispielausgabe:
```
523
Fehler: Sensorwert außerhalb des Bereichs.
761
```

## Vertiefung

Die Fehlerbehandlung war nicht immer so geradlinig. In den frühen Tagen ignorierten Entwickler oft Fehler, was zum gefürchteten "undefinierten Verhalten“ führte. Mit der Evolution der Programmierung entwickelten sich auch die Werkzeuge - in vielen Sprachen gibt es jetzt Ausnahmen, aber in der Arduino-Welt bleibt es aufgrund von Hardwarebeschränkungen und C++-Wurzeln immer noch beim alten 'erst-überprüfen'.

In der Arduino-Programmierung sieht man oft `if-else`-Anweisungen für die Fehlerbehandlung. Es gibt jedoch Alternativen: die Verwendung der `assert`-Funktion, um die Ausführung zu stoppen, wenn eine Bedingung fehlschlägt, oder das Design von Ausfallsicherungen innerhalb Ihrer Hardwarekonfiguration.

Wenn Sie eine Fehlerbehandlung implementieren, sollten Sie die Auswirkungen des Anhaltens des Programms im Vergleich zum Fortfahren mit einem standardmäßigen oder sicheren Zustand abwägen. Es gibt einen Kompromiss, und die richtige Wahl hängt von dem potenziellen Schaden von Unterbrechungen im Vergleich zu einem falschen Betrieb ab.

## Siehe auch

Vertiefen Sie Ihre Kenntnisse in Fehlererkennung und -behandlung mit diesen Ressourcen:

- Referenz der Arduino-Sprache: https://www.arduino.cc/reference/en/
- Ein tiefergehender Blick in die Fehlerbehandlung von Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Fehlerbehandlung in C++: https://en.cppreference.com/w/cpp/error/exception

Dies sollte Ihnen das Wissen und die Zuversicht geben, die Fallstricke von Fehlern bei Ihren Arduino-Abenteuern zu vermeiden.
