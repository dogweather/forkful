---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Arduino-Programmierung: Zeichenketten miteinander verbinden

## Was & Warum?
Das Verbinden von Zeichenketten, bekannt als Konkatenierung, ist der Prozess des Zusammenfügung zweier oder mehrerer Zeichenketten. Es ist nützlich, wenn Programmierer verschiedene Daten zu einer einzigen Zeichenkette zusammenfassen möchten.

## So geht's:

Im Arduino (die aktuelle Version) können wir die `concat()`-Funktion verwenden, um Zeichenketten zu konkatenieren. Hier ist ein Beispiel:

```Arduino
String str1 = "Hallo, ";
String str2 = "Welt!";
str1.concat(str2);
Serial.println(str1);  // Gibt "Hallo, Welt!" aus.
```

Die `concat()`-Funktion lässt zwei Zeichenketten zu einer einzigen verschmelzen. Nachdem die `concat()`-Funktion aufgerufen wurde, ist `str1` jetzt "Hallo, Welt!".

## Tieferer Einblick

Historisch gesehen war in den frühen Tagen der Programmierung das Verketten von Zeichenketten eine ziemlich ressourcenintensive Aufgabe. Heutzutage machen effiziente Funktionen und Bibliotheken wie `concat()` in Arduino diese Aufgabe trivial.

Alternativ zur `concat()`-Funktion können Sie auch den `+` Operator verwenden:

```Arduino
String str1 = "Hallo, ";
String str2 = "Welt!";
String str3 = str1 + str2;
Serial.println(str3);  // Gibt "Hallo, Welt!" aus.
```

Dies lässt `str3` zur kombinierten Zeichenkette werden, während `str1` und `str2` unverändert bleiben. Beachten Sie jedoch, dass bei großen Zeichenketten oder bei der Konkatenierung in Schleifen die Verwendung von `concat()` gegenüber dem `+` Operator aufgrund der besseren Speicherverwaltung bevorzugt wird.

## Siehe auch

Für weitere Informationen, schauen Sie sich die offizielle Arduino-Referenz an:
- Arduino String `concat()` Funktion: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- Arduino String `+` Operator: [https://www.arduino.cc/reference/en/language/variables/data-types/string/operators/plus/](https://www.arduino.cc/reference/en/language/variables/data-types/string/operators/plus/)