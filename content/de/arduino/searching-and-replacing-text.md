---
title:                "Textsuche und -ersetzung"
html_title:           "Arduino: Textsuche und -ersetzung"
simple_title:         "Textsuche und -ersetzung"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textsuche und -ersetzung ist eine nützliche Programmierfunktion, mit der Text innerhalb von Code leicht gefunden und modifiziert werden kann. Programmierer verwenden diese Funktion, um fehlerhafte Texte zu korrigieren, redundante Wörter zu entfernen oder Texte für unterschiedliche Bedingungen anzupassen.

## Wie?
Eine Textsuche und -ersetzung kann in Arduino mit der Funktion `replace()` durchgeführt werden. Diese Funktion nimmt zwei Argumente: das zu ersetzende Wort und das neue Wort. Zum Beispiel:
```Arduino
String text = "Hallo Welt!";
text.replace("Hallo", "Guten Tag");
Serial.println(text);
```
Dieses Beispiel wird "Guten Tag Welt!" ausgeben. Falls mehrere Vorkommnisse des gesuchten Worts vorhanden sind, werden alle ersetzt.

## Tiefere Einblicke
Die Funktion `replace()` ist eine besonders bequeme Möglichkeit, Text in Arduino zu ersetzen. Eine alternative Methode wäre die Verwendung von `str.replace()`, die in C++ verfügbar ist. Diese Funktion hat jedoch eine andere Syntax und erfordert eine Neuzuweisung des modifizierten Textes. Die `replace()`-Funktion von Arduino hingegen ersetzt den Text direkt im ursprünglichen Objekt.

## Siehe auch
Weitere Informationen zu `replace()` und anderen nützlichen Funktionen in Arduino finden Sie in der offiziellen [Dokumentation](https://www.arduino.cc/reference/en/language/functions/communication/stringfunctions/replace/). Weitere Tipps und Tricks zur Textsuche und -ersetzung können Sie auch in [diesem Artikel](https://www.arduino.cc/reference/en/language/functions/communication/stringfunctions/replace/) nachlesen.