---
title:                "Ausgabe von Debug-Meldungen"
html_title:           "Gleam: Ausgabe von Debug-Meldungen"
simple_title:         "Ausgabe von Debug-Meldungen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Debugging-Ausgaben sind Textnachrichten, die während der Ausführung eines Programms gedruckt werden und dabei helfen, Fehler oder Probleme zu identifizieren. Programmierer verwenden sie oft, um zu verstehen, was in ihrem Code passiert und warum bestimmte Probleme auftreten.

## Wie geht es?
Es gibt mehrere Möglichkeiten, Debugging-Ausgaben in Gleam zu erstellen. Eine Möglichkeit ist die Verwendung des `debug`-Moduls, das eine `debug.print`-Funktion bereitstellt. Zum Beispiel:

```Gleam
import debug

debug.print("Hello World")
```

Dies würde die Nachricht "Hello World" in der Konsole ausgeben. Eine andere Möglichkeit ist die Verwendung von `debug.expect`, um zu überprüfen, ob ein bestimmter Wert erwartet wird. Zum Beispiel:

```Gleam
import debug

let a = 1 + 2
debug.expect(a, 3)
```

Dies würde eine Fehlermeldung ausgeben, wenn `a` nicht den erwarteten Wert von 3 hat.

## Tiefentauchen
Das Drucken von Debugging-Ausgaben ist eine gängige Praxis beim Debuggen von Software und hat eine lange Geschichte. Eine Alternative zu diesem Ansatz ist das Verwenden von integrierten Debugging-Tools oder das Verfolgen von Logs. Die Implementierung von Debugging-Ausgaben in Gleam erfolgt durch die Verwendung von Funktionen aus dem `debug`-Modul, die eng mit dem Standardmodul `io` zusammenarbeiten.

## Siehe auch
- [Gleam Handbuch zu Debugging](https://gleam.run/book/introduction.html#debugging)
- [Gleam Dokumentation zu Debugging](https://gleam.run/packages/gleam_stdlib/latest/debug.html)
- [Gleam Standardbibliothek: Debug-Modul](https://gleam.run/packages/gleam_stdlib/latest/debug.html)