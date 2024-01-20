---
title:                "Einen String großschreiben"
html_title:           "Arduino: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein String groß schreiben bedeutet, alle Buchstaben in Großbuchstaben umzuwandeln. Programmierer machen das meistens, um Text einheitlicher und leichter lesbbar zu machen.

## So geht's:

Hier sind einige Codebeispiele mit dazugehöriger Ausgabe.

```Arduino
String str = "programmierung ist cool"; 
str.toUpperCase();
Serial.println(str); 
```

Ausgabe:

```Arduino
PROGRAMMIERUNG IST COOL
```

## Vertiefung

Im historischen Kontext hat das Großschreiben von Zeichenstrings seine Wurzeln in der frühen Computergeschichte, als Großbuchstaben die Norm waren. Als Alternativen können Programmierer Funktionen wie `toUpperCase()` in anderen Programmiersprachen wie Java oder Python verwenden. Das Großschreiben eines Strings in Arduino passiert nicht in-place; eine Kopie des Strings wird erstellt und dann geändert. Dies bedeutet, dass der ursprüngliche String nicht verändert wird.

## Siehe Auch

Für weitere Informationen und verwandte Themen können Sie die folgenden Ressourcen besuchen:

- [Arduino String Manipulation](https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/touppercase)
- [Arduino Strings](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [Programming Languages: A Deep Dive](https://www.codecademy.com/learn/learn-programming)