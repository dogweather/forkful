---
title:                "Arduino: Test schreiben"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben von Tests ist ein wichtiger Bestandteil des Arduino-Programmierens. Durch das Schreiben von Tests können potenzielle Fehler im Code frühzeitig erkannt und behoben werden, bevor sie zu größeren Problemen führen. Außerdem hilft es dabei, die Funktionalität des Codes zu überprüfen und sicherzustellen, dass alle gewünschten Ergebnisse erzielt werden.

# Wie geht es

Das Schreiben von Tests in Arduino kann auf verschiedene Arten erfolgen. Eine häufig verwendete Methode ist die Verwendung von Bibliotheken wie "rttest" oder "Unity", die speziell für das Testen von Arduino-Code entwickelt wurden. Diese Bibliotheken bieten Funktionen zum Vergleichen von Werten, Überprüfen von Bedingungen und Ausgeben von Testergebnissen.

Das folgende Beispiel zeigt, wie ein Test mit der Bibliothek "rttest" aussehen könnte:

```Arduino
#include <rttest.h>

void setup() {
    Serial.begin(9600);
}

void loop() {
    int result = add(2, 3); // Funktion, die getestet werden soll
    RTTest::AssertEqual(result, 5); // Vergleich des Ergebnisses mit erwartetem Wert
    RTTest::PrintResult(); // Ausgabe des Testergebnisses
    while (true) {} // Endlosschleife, um den Test nur einmal auszuführen
}
```

Die Ausgabe des Testergebnisses würde in diesem Fall "PASSED" sein, da die Funktion "add" den erwarteten Wert von 5 zurückliefert.

# Tief tauchen

Beim Schreiben von Tests ist es wichtig, alle möglichen Fälle und Randbedingungen zu berücksichtigen, um sicherzustellen, dass der Code robust und fehlerfrei ist. Auch das Debuggen von Tests kann dabei helfen, mögliche Fehler im Code aufzudecken.

Außerdem ist es sinnvoll, Tests im Laufe der Entwicklung des Codes regelmäßig auszuführen, um sicherzustellen, dass keine neuen Fehler hinzugefügt wurden oder bestehende behoben wurden.

# Siehe auch

- [rttest Bibliothek](https://github.com/werktag/rttest)
- [Unity Bibliothek](https://github.com/ThrowTheSwitch/Unity)