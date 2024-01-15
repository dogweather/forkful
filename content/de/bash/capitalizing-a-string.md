---
title:                "String großschreiben"
html_title:           "Bash: String großschreiben"
simple_title:         "String großschreiben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum?

Es gibt mehrere Gründe, warum man in der Bash-Programmierung eine Zeichenkette groß schreiben würde. Es kann zum Beispiel Teil einer Textmanipulation sein oder um die Lesbarkeit des Codes zu verbessern.

## Anleitung

Um eine Zeichenkette in der Bash groß zu schreiben, können Sie den Befehl "tr" verwenden. Hier ist ein Beispiel, um den Satz "Hallo, Welt!" in Großbuchstaben zu schreiben:

```Bash 
echo "Hallo, Welt!" | tr '[:lower:]' '[:upper:]'
# Ausgabe: HALLO, WELT!
```

Sie können auch die Funktion "toupper" verwenden, die in der Bash 4.0 und höher verfügbar ist. Hier ist ein Beispiel:

```Bash
text="Hallo, Welt!"
echo "${text^^}"
# Ausgabe: HALLO, WELT!
```

## Tiefere Einblicke

In der Bash gibt es keine eigene Funktion, um eine Zeichenkette in Großbuchstaben zu schreiben. Stattdessen können Sie die genannten Methoden verwenden, um eine Zeichenkette aus Kleinbuchstaben in Großbuchstaben umzuwandeln. Die Verwendung von "tr" ist jedoch nicht nur auf Groß- und Kleinbuchstaben beschränkt, sondern kann auch für andere Zeichen und Zeichensätze verwendet werden.

## Siehe auch

- Die offizielle Dokumentation zu [tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation)
- Ein praktisches Beispiel für die Verwendung von "tr" in der Bash [hier](https://www.baeldung.com/linux/convert-uppercase-lowercase#tr-command)