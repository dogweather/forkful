---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Drucken von Debug-Ausgaben ist ein Prozess, bei dem Programmierer Daten auf eine Konsole oder serielle Schnittstelle ausgeben, um den Zustand und das Verhalten ihres Code zu überwachen. Es ist nützlich, um unbeabsichtige Fehler im Code zu entdecken und den Fluss der Logik besser zu verstehen.

## So geht's:

In Arduino können wir die Funktion `Serial.println()` verwenden, um Debug-Ausgaben zu drucken. Sie müssen zuerst die serielle Kommunikation mit `Serial.begin() `initialisieren.

```Arduino
void setup() {
  // Starten der seriellen Kommunikation.
  Serial.begin(9600);
}

void loop() {
  // Wert an seriellen Port senden.
  Serial.println("Hallo Welt");
  delay(1000); // Pause von 1 Sekunde.
}
```

In diesem Beispiel sendet der Arduino jede Sekunde "Hallo Welt" an den Seriellen Port.

## Vertiefung

Das Drucken von Debug-Ausgaben hat eine lange historische Tradition im codierungsdominierten Umfeld und wurde verbessert und angepasst, um auf Mikrocontroller wie Arduino zu passen. Alternativen zum Drucken von Debug-Ausgaben auf Arduino könnten das Schreiben auf einen SD-Karten-Logger oder das Senden von Daten über Netzwerkanforderungen beinhalten, abhängig von der spezifischen Anwendung und den vorhandenen Ressourcen.

In Bezug auf Implementierungsdetails sollte die Geschwindigkeit von `Serial.begin()` auf die Baudrate des Serial Monitor eingestellt sein. Andernfalls können die Ausgaben Sie in verwirrende Charaktergruppen führen.

## Siehe auch:

1. Offizielle Arduino-Referenz zur `Serial` Klasse: [Link](https://www.arduino.cc/reference/en/language/functions/communication/serial/)