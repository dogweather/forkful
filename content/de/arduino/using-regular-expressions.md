---
title:                "Arduino: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von regulären Ausdrücken ist ein effektiver Weg, um in der Programmierung Textmuster zu suchen und zu manipulieren. Dies kann hilfreich sein, um bestimmte Informationen aus einer größeren Menge von Text herauszufiltern oder um Daten zu validieren. Mit dem Arduino können wir auch reguläre Ausdrücke nutzen, um unsere Projekte noch leistungsfähiger zu gestalten.

## Anleitung

Um reguläre Ausdrücke auf dem Arduino zu verwenden, folgen Sie den folgenden Schritten:

1. Importieren Sie die "regex" Bibliothek in Ihrem Sketch: ```Arduino#include <regex.h>```
2. Erstellen Sie ein "regex" Objekt, indem Sie den gewünschten Ausdruck und die passenden Optionen angeben: ```Arduinoregex myRegex("pattern", options);```
3. Verwenden Sie die "search" oder "match" Methode, um den Ausdruck auf einen Text anzuwenden: ```Arduinobool found = myRegex.search(text);```
4. Überprüfen Sie das Ergebnis und führen Sie entsprechende Aktionen durch: ```Arduinoif (found) { // do something }```

Hier ist ein Beispiel, um eine E-Mail-Adresse aus einer Zeichenkette zu extrahieren und auf dem seriellen Monitor auszugeben:

```
#define EMAIL_PATTERN "[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,3}"
regex emailRegex(EMAIL_PATTERN);
String text = "Meine E-Mail-Adresse ist max.mustermann@example.com";
if (emailRegex.match(text)) {
  Serial.println(emailRegex.matched()); // gibt "max.mustermann@example.com" aus
}
```

## Tiefer Einblick

- Reguläre Ausdrücke bestehen aus einer Kombination von Zeichen, die ein bestimmtes Muster beschreiben. Sie können auch mit Variablen und anderen Textmanipulationsfunktionen kombiniert werden, um komplexe Muster zu erstellen.
- Die "match" Methode gibt nicht nur das passende Muster zurück, sondern auch Informationen über die Position und die Anzahl der Übereinstimmungen.
- Es gibt verschiedene Optionen, die beim Erstellen eines "regex" Objekts angegeben werden können, um das Verhalten des Ausdrucks zu steuern, z.B. die Beachtung von Groß- und Kleinschreibung oder die Behandlung von Leerzeichen.
- Für komplexe Anwendungen können sogenannte "Capture Groups" verwendet werden, um Teile des Textes zu isolieren, die auf ein bestimmtes Muster passen.

## Siehe auch

- [Regex-Tutorial für Arduino](https://www.arduino.cc/reference/en/language/functions/regular-expressions/)
- [Online RegEx Tester](https://regex101.com/)
- [RegEx Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)