---
title:                "Gleam: Debug-Ausgaben drucken"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Methoden, um die Fehler in unserem Code zu finden und zu beheben. Eine einfache, aber effektive Methode ist das Ausgeben von Debug-Informationen. Das bedeutet, dass wir während der Ausführung unseres Codes zusätzliche Informationen anzeigen, um Probleme zu lokalisieren und zu beheben. In diesem Blog-Beitrag werden wir uns ansehen, wie wir Debug-Ausgaben in Gleam verwenden können.

## Wie geht das?

Um Debug-Ausgaben in Gleam zu erstellen, verwenden wir die Funktion `debug/1`. Diese Funktion akzeptiert einen beliebigen Wert und gibt ihn in der Konsole aus. Schauen wir uns ein Beispiel an:

```Gleam
let message = "Hallo, Welt!"

debug(message) // gibt "Hallo, Welt!" in der Konsole aus
```

Wenn wir uns das oben genannte Beispiel ansehen, können wir sehen, dass die Syntax sehr einfach ist. Wir geben einfach den Wert an, den wir in der Konsole ausgeben möchten, innerhalb der `debug` Funktion. Wir können dies auch mit komplexeren Werten wie Listen oder Tupeln tun:

```Gleam
let numbers = [1, 2, 3]

debug(numbers) // gibt [1, 2, 3] in der Konsole aus
```

## Tiefer ins Detail gehen

Das Ausgeben von Debug-Informationen ist eine großartige Möglichkeit, um zu verstehen, was in unserem Code passiert. Es kann jedoch auch nützlich sein, um spezifische Informationen über unsere Variablen oder Funktionen zu erhalten. In diesem Fall verwenden wir die Funktion `inspect/1`, die akzeptiert ebenfalls einen beliebigen Wert, aber gibt eine lesbare Repräsentation dieses Wertes zurück. Schauen wir uns ein Beispiel an:

```Gleam
let name = "Max"

inspect(name) // gibt "String(\"Max\")" in der Konsole aus
```

Wie Sie sehen können, gibt die Funktion `inspect` eine lesbare Repräsentation des übergebenen Wertes zurück. Dies kann hilfreich sein, um genau zu verstehen, was in unserer Variablen gespeichert ist.

## Siehe auch

- [Gleam-Dokumentation zur Debug-Ausgabe](https://gleam.run/documentation/debugging#output)
- [Ein weiterer Blog-Beitrag über Gleam-Debugging-Techniken](https://blog.example.com/gleam-debugging)
- [Gleam-Community-Diskussionsforum über Debugging](https://forum.gleam.run/c/debugging)