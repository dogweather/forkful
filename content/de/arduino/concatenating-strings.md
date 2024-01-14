---
title:    "Arduino: Zeichenketten verbinden"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen oder Verketten von Zeichenfolgen ist ein wichtiges Konzept in der Arduino-Programmierung, da es ermöglicht, verschiedene Zeichenfolgen miteinander zu kombinieren und so komplexe Ausgaben zu erzeugen.

## How To

Um Zeichenfolgen in Arduino zu verketten, nutzen wir den "+" Operator. Schauen wir uns ein Beispiel an:

```Arduino
String firstName = "Max";
String lastName = "Mustermann";
String fullName = firstName + " " + lastName;
Serial.println(fullName);
```

Dieses Beispiel würde die Zeichenfolge "Max Mustermann" ausgeben. Beachte, dass wir den "+" Operator zwischen den einzelnen Zeichenfolgen setzen. Dadurch werden sie miteinander verbunden.

Eine weitere Möglichkeit ist die Verwendung der `concat()` Funktion. Diese erlaubt die Verkettung von mehr als zwei Zeichenfolgen und wird wie folgt genutzt:

```Arduino
String name = "John";
String middleName = "F.";
String lastName = "Kennedy";
name.concat(" ").concat(middleName).concat(" ").concat(lastName);
Serial.println(name);
```

Dieses Beispiel würde die Zeichenfolge "John F. Kennedy" ausgeben.

## Deep Dive

Um Zeichenfolgen effizient zu verketten, ist es wichtig zu verstehen, wie dieses Konzept im Hintergrund funktioniert. In Arduino werden Zeichenfolgen als Objekte der Klasse `String` behandelt. Der "+" Operator wird von dieser Klasse überschrieben, um Zeichenfolgen zu verketten.

Es ist jedoch wichtig zu beachten, dass das Verketten von Zeichenfolgen auf diese Weise nicht die performanteste Methode ist, da jedes Mal ein neues Objekt erstellt werden muss. Für Programme mit vielen Zeichenfolgen kann dies zu einer Verschwendung von Speicher führen. Eine bessere Alternative ist die Verwendung von Zeichenarrays und Funktionen wie `strcat()`.

## Siehe auch

Hier sind einige weitere hilfreiche Ressourcen zum Thema "Concatenating Strings" in der Arduino-Programmierung:

- (https://www.arduino.cc/en/Reference/StringObject)
- (https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)