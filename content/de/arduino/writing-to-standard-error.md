---
title:                "Das Schreiben auf Standardfehler"
html_title:           "Arduino: Das Schreiben auf Standardfehler"
simple_title:         "Das Schreiben auf Standardfehler"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

In jedem Code kommt es vor, dass etwas schief geht. In solchen Situationen ist es hilfreich, Fehlermeldungen zu erhalten, um das Problem schnell zu beheben. Das Schreiben von Fehlermeldungen in den Standardfehlerkanal ermöglicht es uns, genauere Informationen über den Fehler zu erhalten und unseren Code zu verbessern.

## Wie es geht

Um eine Fehlermeldung in den Standardfehlerkanal zu schreiben, verwenden Sie die Funktion ```Serial1.println()```. Hier ist ein Beispiel:

```
Arduino.println("Fehler: Ungültiger Wert");
```

Dies schreibt die angegebene Fehlermeldung in den Standardfehlerkanal und gibt sie auf dem Seriellen Monitor aus.

## Tiefere Einblicke

Beim Schreiben von Fehlern in den Standardfehlerkanal ist es wichtig zu beachten, dass dies nicht den Code stoppt und auch nicht die Ausführung unterbricht. Es ist lediglich ein Mittel, um zusätzliche Informationen über mögliche Fehler zu erhalten.

Es ist auch möglich, Fehlercodes oder Variablenwerte in den Standardfehlerkanal zu schreiben, um mehr Details über den Fehler zu erhalten. Dies kann besonders hilfreich sein, wenn der Fehler nur sporadisch auftritt und schwer zu reproduzieren ist.

# Siehe auch

- [Arduino Dokumentation](https://www.arduino.cc/reference/en/)
- [Einen Fehlerbehandlungsmechanismus implementieren](https://www.arduino.cc/en/Tutorial/Debugging)
- [Serielle Kommunikation mit dem Arduino](https://www.arduino.cc/en/Serial/)