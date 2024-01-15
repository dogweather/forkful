---
title:                "Verwendung regulärer Ausdrücke"
html_title:           "Arduino: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit regulären Ausdrücken beschäftigen? Ganz einfach: Sie vereinfachen das Suchen und Ersetzen von Text in Programmen und machen die Verarbeitung von Daten flexibler.

## Wie man reguläre Ausdrücke in Arduino verwendet
Reguläre Ausdrücke in Arduino werden mithilfe der eingebauten "Regexp" Bibliothek verwendet. Zunächst muss diese Bibliothek im Sketch eingelesen werden:

```Arduino
#include <Regexp.h> 
```

Als nächstes definieren wir einen String, auf den wir den regulären Ausdruck anwenden möchten:

```Arduino
String text = "Dies ist ein Beispieltext zum Testen von regulären Ausdrücken.";
```

Nun können wir mithilfe der `Regexp` Funktionen den Text durchsuchen und bearbeiten. Hier sind einige Beispiele:

### Suchen und Ersetzen
Um bestimmte Wörter oder Zeichenfolgen in einem Text zu finden und zu ersetzen, verwenden wir die `regreplace()` Funktion. Hier ist ein Beispiel, bei dem wir alle Vorkommen von "Beispiel" durch "Test" ersetzen:

```Arduino
char pattern[] = "Beispiel";
char replacement[] = "Test";
regreplace(text, pattern, replacement);
```

Die geänderte Version des Textes wird nun in der Variablen `text` gespeichert.

### Extrahieren von Daten
Reguläre Ausdrücke ermöglichen es auch, Daten aus einem String zu extrahieren. Nehmen wir an, wir haben einen Text mit einer Liste von Namen und Telefonnummern und möchten nur die Telefonnummern extrahieren. Dazu verwenden wir die Funktion `regfind()`:

```Arduino
char pattern[] = "\\d{10}"; // Telefonnummern bestehen aus 10 Ziffern
String phoneNumber = regfind(text, pattern); // Extrahiert die erste 10-stellige Ziffernfolge im Text
```

### Überprüfen von Bedingungen
Mithilfe von regulären Ausdrücken können auch Bedingungen überprüft werden. Zum Beispiel können wir prüfen, ob eine E-Mail-Adresse gültig ist, indem wir nach einem bestimmten Muster suchen. Hier ist ein Beispiel, das überprüft, ob eine String-Variable `email` eine gültige E-Mail-Adresse enthält:

```Arduino
char pattern[] = "\\w+@[a-z]+\\.[a-z]+";
if (regfound(email, pattern)) {
  // E-Mail ist gültig
}
```

## Tiefergehende Informationen über reguläre Ausdrücke
Reguläre Ausdrücke können kompliziert werden, aber es gibt viele Online-Ressourcen und Tutorials, die Ihnen bei der Erstellung und Verwendung helfen können. Ein guter Ausgangspunkt ist die offizielle [Arduino-Referenz](https://www.arduino.cc/reference/en/libraries/regexp/) für die Regexp Bibliothek. Zudem gibt es viele nützliche Tutorials auf Plattformen wie [Instructables](https://www.instructables.com/circuits/arduino/projects/) oder [Arduino Project Hub](https://create.arduino.cc/projecthub/projects/tags/regular%20expressions).

## Siehe auch
- [Arduino-Referenz für die Regexp Bibliothek](https://www.arduino.cc/reference/en/libraries/regexp/)
- [Reguläre Ausdrücke bei Instructables](https://www.instructables.com/circuits/arduino/projects/)
- [Projekte mit regulären Ausdrücken auf Arduino Project Hub](https://create.arduino.cc/projecthub/projects/tags/regular%20expressions)