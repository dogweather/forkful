---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Arduino: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Warum: 
Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Arduino Programmierung nützlich sein, um zum Beispiel ungewollte Eingaben bei der Verwendung von Sensoren zu filtern oder um Daten effizienter zu verarbeiten.

How To: 
Das Löschen von Zeichen in Arduino ist recht einfach und erfordert nur wenige Zeilen Code. Hier ist ein Beispiel, wie man alle Zeichen löschen kann, die dem Buchstaben 'a' entsprechen:

```Arduino
// Deklaration und Initialisierung des Zeichenarrays
char data[] = "Hallo Arduino";
// Variable zur Speicherung der Länge des Arrays
int len = sizeof(data);
// Schleife zum Durchlaufen des Arrays
for (int i = 0; i < len; i++) {
  // Überprüfung ob aktuelles Zeichen dem Muster entspricht
  if (data[i] == 'a') {
    // Falls ja, wird das Zeichen aus dem Array gelöscht
    memmove(data + i, data + i + 1, len - i);
    // Da ein Zeichen gelöscht wurde, wird die Schleife erneut mit dem aktuellen Index durchlaufen
    i--;
  }
}
// Ausgabe des geänderten Arrays
Serial.println(data);
```

Der obige Code nimmt das Zeichenarray "Hallo Arduino" und entfernt alle Zeichen, die dem Buchstaben 'a' entsprechen. Die Ausgabe ist dann "Hllo Arduino".

Deep Dive:
Die Funktion `memmove()` wird in diesem Beispiel verwendet, um das Zeichen aus dem Array zu löschen. Sie gehört zur C Standardbibliothek und ermöglicht es, einen Teil des Speichers zu verschieben. Im Code wird sie verwendet, um das Zeichen an der aktuellen Position durch das nächste im Array stehende Zeichen zu ersetzen. Dadurch wird das Zeichen effektiv "gelöscht". Die Schleife läuft für jedes gelöschte Zeichen erneut mit dem aktuellen Index durch, um sicherzustellen, dass keines der verschobenen Zeichen übersprungen wird.

See Also:
Hier sind einige nützliche Links für weitere Informationen zur Arduino Programmierung und zur Verwendung von Strings und Zeichenarrays:

- [Arduino Language Reference](https://www.arduino.cc/reference/en/)
- [C String Tutorial](https://www.arduino.cc/reference/en/language/variables/data-types/string/manipulation/)
- [Characters and Strings in C](https://www.cprogramming.com/tutorial/c/lesson9.html)