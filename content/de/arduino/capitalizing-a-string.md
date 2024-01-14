---
title:                "Arduino: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum
Die Verwendung von Groß- und Kleinschreibung ist ein wichtiger Bestandteil beim Programmieren in Arduino. Es kann dabei helfen, Variablen und Funktionsnamen deutlicher zu machen und die Lesbarkeit des Codes zu verbessern.

# Wie geht es
Um eine Zeichenkette in Arduino zu kapitalisieren, gibt es verschiedene Möglichkeiten. Eine Möglichkeit besteht darin, den `toUpperCase()` Befehl zu verwenden, der eine Zeichenkette in Großbuchstaben umwandelt.

```Arduino
String text = "hallo welt";
text.toUpperCase();
```

Dieser Code würde "HALLO WELT" ausgeben. Eine weitere Möglichkeit besteht darin, die Funktion `capitalize()` zu verwenden, die nur den ersten Buchstaben der Zeichenkette in einen Großbuchstaben umwandelt.

```Arduino
String text = "hallo welt";
text.capitalize();
```

Dieser Code würde "Hallo welt" ausgeben.

# Tiefer eintauchen
Um tiefer in das Thema zu gehen, ist es wichtig zu verstehen, dass Zeichenketten in Arduino als Objekte behandelt werden und daher über verschiedene Funktionen und Methoden verfügen. Zum Beispiel können Sie mit der `charAt()`-Funktion auf einen bestimmten Buchstaben in der Zeichenkette zugreifen und ihn in einen Groß- oder Kleinbuchstaben umwandeln.

```Arduino
String text = "hallo welt";
text.charAt(0) = 'H';
```

Dieser Code würde den ersten Buchstaben in "hallo welt" in einen Großbuchstaben umwandeln und die Ausgabe wäre "Hallo welt".

# Siehe auch
- [String Objekt in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [toUpperCase() Funktion](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [capitalize() Funktion](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/capitalize/)