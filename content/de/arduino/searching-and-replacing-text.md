---
title:                "Suchen und Ersetzen von Text"
html_title:           "Arduino: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Wenn du viel mit Texten in deiner Arduino Programmierung zu tun hast, wirst du früher oder später auf das Problem stoßen, dass du bestimmte Textpassagen in deinem Code ändern oder ersetzen musst. Das manuelle Durchsuchen und Ersetzen von Text kann dabei sehr zeitaufwändig und fehleranfällig sein. In solchen Fällen ist es sinnvoll, eine effiziente Methode zum Durchsuchen und Ersetzen von Text zu verwenden.

## Wie es geht
Das Durchsuchen und Ersetzen von Text in Arduino kann mithilfe der `replace()` Funktion in der String Bibliothek durchgeführt werden. Diese Funktion hat zwei Parameter: den zu suchenden Text und den Text, mit dem er ersetzt werden soll. Hier ist ein Beispiel, wie du diese Funktion verwenden kannst:

```Arduino
String text = "Hallo Welt!";
text.replace("Welt", "Arduino");

Serial.println(text); // Gibt "Hallo Arduino!" aus
```

## Tiefer eintauchen
Die `replace()` Funktion kann auch mit Variablen verwendet werden. Wenn du beispielsweise einen bestimmten Text in einer Benutzereingabe suchen und ersetzen möchtest, kannst du Variablen verwenden, um die Eingabe zu speichern und dann mit der `replace()` Funktion zu manipulieren.

Hier ist ein Beispiel dafür:

```Arduino
String input;
String search = "hund";
String replace = "katze";

// Benutzereingabe speichern
input = Serial.readString();

// Text durchsuchen und ersetzen
input.replace(search, replace);

Serial.println(input); // Gibt die veränderten Textausgabe aus
```

Es ist auch wichtig zu beachten, dass die `replace()` Funktion nur den ersten Treffer des gesuchten Textes ersetzt. Wenn du alle Treffer innerhalb eines Strings ersetzen möchtest, kannst du die Funktion in einer Schleife verwenden.

## Siehe auch
- [String Bibliothek Referenz](https://www.arduino.cc/en/Reference/String)
- [Einführung in die String Manipulation mit Arduino](https://maker.pro/arduino/tutorial/how-to-use-string--manipulation-with-arduino)
- [Einführung in die Programmierung mit Arduino](https://www.youtube.com/watch?v=oJ1gKDGGvg0)