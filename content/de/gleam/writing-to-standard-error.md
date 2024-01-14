---
title:    "Gleam: Schreiben auf Standardfehler"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Fehlermeldungen ist eine wichtige Fähigkeit für jeden Programmierer, da es die Zuverlässigkeit und Fehlerbehebung von Programmen verbessert.

## Wie funktioniert es?

Die Verwendung der `write_stderr` Funktion in Gleam ermöglicht es, Fehlermeldungen an die Standardfehlerausgabe zu übergeben. Hier ist ein Beispielcode in Gleam:

```Gleam
import gleam/log

let message = "Ein Fehler ist aufgetreten!"

log.write_stderr(message)
```

Die Ausgabe sieht wie folgt aus:

```
Ein Fehler ist aufgetreten!
```

## Tiefer Eintauchen

Es gibt verschiedene Gründe, warum das Schreiben von Fehlern an die Standardfehlerausgabe sinnvoll ist. Zum einen ermöglicht es eine effektive Fehlerbehandlung, da Fehlermeldungen direkt auf der Konsole angezeigt werden und somit leichter zu erkennen sind. Zum anderen kann die Standardfehlerausgabe auch für Debugging-Zwecke genutzt werden, um spezifische Informationen zu bestimmten Aspekten des Programms zu erhalten.

In Gleam können auch Formatierungsoptionen wie Farben und Zeilenumbrüche verwendet werden, um die Lesbarkeit von Fehlermeldungen zu verbessern. Hier ist ein Beispielcode:

```Gleam
import gleam/log

let message = "Dies ist eine [red]Error-Nachricht[/] mit einem\nZeilenumbruch."

log.write_stderr(message)
```

Und die Ausgabe sieht so aus:

```
Dies ist eine Error-Nachricht mit einem
Zeilenumbruch.
```

## Siehe auch

- Offizielle Dokumentation zu Gleam: [https://gleam.run/](https://gleam.run/)
- "Einführung in die Fehlerbehandlung in Gleam" (Englisch): [https://gleam.run/book/case\_studies/logging\_errors.html](https://gleam.run/book/case_studies/logging_errors.html)
- "Debugging von Gleam-Code" (Englisch): [https://gleam.run/blog/debugging.html](https://gleam.run/blog/debugging.html)