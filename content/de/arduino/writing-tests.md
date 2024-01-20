---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Testen ist ein wichtiger Bestandteil der Programmierung, bei dem der Code auf mögliche Fehler und Fehlerquellen überprüft wird. Es hilft Programmierern, sicherzustellen, dass ihr Code zuverlässig funktioniert und die erwarteten Ergebnisse liefert.

## So geht's:
Um Tests für Arduino-Code zu schreiben, können wir die ```ArduinoUnit.h``` Bibliothek nutzen. Hier ein einfaches Beispiel, das eine Testklasse für eine einfache Funktion erstellt:

```
#include <ArduinoUnit.h>

test(function_test) {
  assertEqual(2, addNumbers(1,1));
}

void addNumbers(int a, int b) {
  return a+b;
}
```

Nachdem wir die Bibliothek eingebunden haben, können wir eine Testklasse mit dem Namen "function_test" erstellen, in der wir unsere Funktion testen wollen. Mit dem ```assertEqual()``` Befehl prüfen wir, ob die erwarteten Werte übereinstimmen. Anschließend definieren wir unsere Funktion und können mit ```return``` den Rückgabewert überprüfen.

Die Ausgabe sollte nun "1 test, 1 passed, 0 failed" sein, was bedeutet, dass unser Test erfolgreich war.

## Tiefentauchen:
Die ArduinoUnit.h Bibliothek wurde von M. F. Mc Laughlin erstellt und ist eine Alternative zu anderen Test-Frameworks wie beispielsweise Unity oder CppUnit. Mit ihr können sowohl Unit-Tests als auch Integrationstests geschrieben werden. Eine ausführlichere Dokumentation und weitere Beispiele findest du auf der offiziellen GitHub-Seite.