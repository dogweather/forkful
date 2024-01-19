---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was und Warum?

Die Umwandlung eines Zeichenkettens in Kleinbuchstaben, bezeichnet eine grundlegende Funktion in der Programmierung. Sie wird oft genutzt, um Eingabeunstimmigkeiten zu beseitigen und so den Datenabgleich zu vereinfachen. 

## Wie macht man das:

Hier ist ein einfaches Beispiel in Arduino zum Umwandeln einer Zeichenkette in Kleinbuchstaben:
```Arduino
void setup() {
  Serial.begin(9600);
  String str = "Hallo Welt!";
  str.toLowerCase();
  Serial.println(str);
}

void loop() {} 
```

Die Ausgabe wird sein:

```Arduino
"hallo welt!"
```

## Vertiefung

In den alten Zeiten der Programmierung, waren die Computersysteme nicht immer in der Lage, Groß- und Kleinbuchstaben zu unterscheiden. Daher mussten Programmierer manchmal explizit Zeichenketten in ein einheitliches Format, wie Kleinbuchstaben, umwandeln.

Es gibt auch Alternativen zur `toLowerCase()` Funktion. Man kann z.B. einen einfachen Algorithmus verwenden, der durch jedes Zeichen der Zeichenkette geht und dieses in einen Kleinbuchstaben umwandelt. Aber in Arduino ist `toLowerCase()` die einfachste und effizienteste Methode.

Die Umwandlung einer Zeichenkette in Kleinbuchstaben in Arduino erfolgt intern durch eine einfache Manipulation der ASCII-Werte der Zeichen. Jedes Großbuchstaben-Zeichen wird in sein Kleinbuchstaben-Äquivalent umgewandelt, indem ein fester Wert hinzugefügt wird.

## Siehe Auch

Für weitere Details über die `toLowerCase()` Funktion, siehe die offizielle Arduino-Dokumentation: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/.

Außerdem ist der ASCII-Zeichensatz ein nützliches Tool, um zu verstehen, wie Zeichen in Computern dargestellt werden: https://www.asciitable.com/.