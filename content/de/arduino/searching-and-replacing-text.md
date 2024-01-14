---
title:                "Arduino: Suchen und Ersetzen von Text"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Austauschen von Text in einem Arduino Programm kann helfen, Fehler zu beheben, das Programm zu optimieren oder es an individuelle Bedürfnisse anzupassen. Es ist auch eine gute Möglichkeit, den Umgang mit Textverarbeitung in der Programmierung zu üben.

## How To

Das Austauschen von Text in einem Arduino Programm kann auf verschiedene Arten durchgeführt werden. Eine Möglichkeit ist die Verwendung der "replace()" Funktion. Hier ein Beispiel:

```Arduino
char text[] = "Hallo Welt";
replace(text, "Welt", "Arduino");
Serial.println(text); // Output: Hallo Arduino
```

Die "replace()" Funktion sucht nach einem bestimmten Text in einer Zeichenkette und ersetzt ihn durch einen neuen Text. In diesem Fall wird "Welt" durch "Arduino" ersetzt.

Eine andere Möglichkeit ist die Verwendung von regulären Ausdrücken mit der "regexReplace()" Funktion. Hier ein Beispiel:

```Arduino
char text[] = "Ich habe 3 Äpfel und 4 Bananen";
regexReplace(text, "\\d", "x");
Serial.println(text); // Output: Ich habe x Äpfel und x Bananen
```

Die "regexReplace()" Funktion sucht nach einer bestimmten Zeichenfolge, die dem regulären Ausdruck entspricht, und ersetzt sie durch den neuen Text. Im obigen Beispiel wird jede Zahl durch "x" ersetzt.

## Deep Dive

Wenn man tiefer in die Thematik des Austauschens von Text eintauchen möchte, gibt es noch weitere Funktionen und Techniken, die hilfreich sein können. Beispielsweise kann die "String" Klasse verwendet werden, um Texte miteinander zu verbinden oder zu ersetzen. Auch die Verwendung von Variablen und Schleifen kann hilfreich sein, um spezifische Textpassagen auszutauschen.

Es ist auch wichtig zu beachten, dass in Arduino das Unicode System verwendet wird, was bedeutet, dass auf Zeichen mit diakritischen Zeichen, wie z.B. "ö", anders zugegriffen werden muss. Hierfür gibt es spezielle Funktionen, wie zum Beispiel "String::replaceChars()", die solche Sonderzeichen korrekt behandeln.

## Siehe auch
- [Arduino String Klasse](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [Regex-Tutorial](https://www.regextutorial.org/)
- [Unicode in Arduino](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/#unicode)