---
title:                "Arduino: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

### Warum

Die Suche und Ersetzung von Text ist eine wichtige Fähigkeit für jeden Programmierer, egal ob Anfänger oder Fortgeschrittener. Durch das Korrigieren von Schreibfehlern oder das Ändern von Variablennamen können wir effizienter arbeiten und unsere Codes übersichtlicher gestalten.

### Wie geht's?

Um Text zu durchsuchen und zu ersetzen, können wir die `replace()` Funktion in Arduino verwenden. Diese Funktion akzeptiert drei Parameter: den zu suchenden Teil des Textes, den zu ersetzenden Teil und den Text, in dem die Suche durchgeführt werden soll. Schauen wir uns ein Beispiel an:

```Arduino
String text = "Hallo Welt, ich bin ein Arduino Programmierer.";
text.replace("Arduino", "Mikrocontroller");
Serial.println(text);
```

Output:
```
Hallo Welt, ich bin ein Mikrocontroller Programmierer.
```

In diesem Beispiel haben wir den Text `Arduino` in `Mikrocontroller` geändert und den neuen Text auf der Seriellen Schnittstelle ausgegeben. Beachte, dass die `replace()` Funktion den Text direkt in der Variable `text` ändert, daher müssen wir keine neue Variable für den geänderten Text deklarieren.

### Tiefer Einblick

Neben der `replace()` Funktion gibt es noch weitere Möglichkeiten, Text in Arduino zu suchen und zu ersetzen. Ein Beispiel ist die Verwendung von regulären Ausdrücken mit der `regexReplace()` Funktion aus der Arduino Regex Bibliothek. Dadurch können wir komplexere Suchanfragen durchführen und beispielsweise alle Zahlen in einem Text durch Sternchen ersetzen.

```Arduino
#include <Regex.h> // Inkludieren der Regex Bibliothek

String text = "5 Äpfel, 4 Birnen, 2 Bananen";
text.regexReplace("[0-9]", "*");
Serial.println(text);
```

Output:
```
* Äpfel, * Birnen, * Bananen
```

Außerdem können wir mit der `indexOf()` Funktion eine bestimmte Position im Text finden und dann mit der `substring()` Funktion einen Teil des Textes ersetzen. Durch das Kombinieren dieser Funktionen können wir z.B. einzelne Wörter oder Sätze in einem längeren Text ersetzen.

### Siehe auch

- [Offizielle Arduino Referenz zu replace()](https://www.arduino.cc/reference/en/language/strings/stringobject/replace/)
- [Einführung zu regulären Ausdrücken in Arduino](https://www.arduino.cc/reference/en/language/functions/communication/regexreplace/)
- [Beispiele mit indexOf() und substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)