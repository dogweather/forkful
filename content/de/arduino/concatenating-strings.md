---
title:                "Arduino: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Verkettung von Strings ist ein wesentlicher Bestandteil der Programmierung. Sie ermöglicht es uns, verschiedene Variablen und Texte zu kombinieren und so komplexe Ausgaben zu erstellen. In diesem Blog-Beitrag werden wir uns genauer damit beschäftigen, warum es wichtig ist, Strings in Arduino-Programmen zu verketten.

## So geht's

In der Arduino-Programmierung können wir Strings auf verschiedene Arten verketten. Eine Möglichkeit ist die Verwendung des `+` Operators, der zwei Strings miteinander verbindet. Wir können auch die `concat()` Funktion nutzen, um mehrere Strings zusammenzufügen.

Um dies besser zu verstehen, werfen wir einen Blick auf ein einfaches Beispiel:

```Arduino
String name = "Max";
String message = "Hallo, mein Name ist " + name + "!";

Serial.println(message);
```

Das Ergebnis unserer Ausgabe wäre: "Hallo, mein Name ist Max!". Hier haben wir den Inhalt der Variablen `name` in unseren Text eingefügt, indem wir den `+` Operator verwendet haben.

Eine alternative Möglichkeit, Strings zu verketten, ist die Verwendung der `printf()` Funktion. Diese ermöglicht es uns, Formatierungszeichen zu nutzen, um die Ausgabe zu gestalten. Hier ein Beispiel:

```Arduino
String first_name = "Anna";
String last_name = "Miller";
int age = 32;

String message = "Mein Name ist %s %s und ich bin %d Jahre alt.";

Serial.printf(message.c_str(), first_name.c_str(), last_name.c_str(), age);
```

Unser Resultat wäre: "Mein Name ist Anna Miller und ich bin 32 Jahre alt." Hier haben wir `%s` verwendet, um unsere String-Variablen einzufügen und `%d`, um unsere numerische Variable `age` einzufügen.

## Tiefere Einblicke

Die Verkettung von Strings ist besonders nützlich, wenn wir komplexe Ausgaben erstellen müssen, z.B. für Benutzeroberflächen oder zur Kommunikation mit anderen Geräten. Es ermöglicht uns auch, unseren Code lesbarer zu gestalten, indem wir die Verwendung von Variablen in unseren Texten präziser machen.

Es ist jedoch wichtig zu beachten, dass die Verkettung von Strings im Vergleich zu anderen Operationen in der Arduino-Programmierung relativ aufwendig ist. Wenn es in unseren Programmen darauf ankommt, die Leistung zu optimieren, sollten wir die Verwendung von Strings, insbesondere in Schleifen, vermeiden.

## Siehe auch

* [Arduino String Referenz](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
* [Die Verwendung von Strings in C und Arduino](https://www.programmingelectronics.com/learn/c/the-difference-between-strings-and-char-arrays/)
* [C++ String-Klasse](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)