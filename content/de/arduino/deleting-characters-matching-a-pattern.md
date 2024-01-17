---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Arduino: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist ein häufig verwendetes Programmierkonzept, um bestimmte Teile eines Texts zu entfernen. Programmierer verwenden dies, um unerwünschte Informationen aus einer Datei oder einem String zu entfernen, um z. B. eine sinnvolle Ausgabe zu erhalten.

## Wie geht's:

```
Arduino code blocks
```

Eine Möglichkeit, um Zeichen, die einem bestimmten Muster entsprechen, zu löschen, ist die Verwendung der `remove_if()` Funktion. Diese Funktion löscht alle Zeichen in einem String, die einer Bedingung entsprechen. Zum Beispiel löscht der folgende Code alle Zahlen aus einem String und gibt das Ergebnis aus:

```
Arduino Beispielcode:
String text = "Die Sonne scheint und es ist 25 Grad";
text.remove_if(isDigit);
Serial.print(text);
// Ausgabe: Die Sonne scheint und es ist Grad
```

Eine andere Möglichkeit, Zeichen zu löschen, ist die Verwendung der `replace()` Funktion. Diese Funktion ersetzt alle Zeichen in einem String, die einem bestimmten Muster entsprechen, durch ein anderes Zeichen. Der folgende Code zeigt, wie man alle Leerzeichen durch Bindestriche ersetzen kann:

```
Arduino Beispielcode:
String text = "Hallo Welt!";
text.replace(" ", "-");
Serial.print(text);
// Ausgabe: Hallo-Welt!
```

## Tiefere Einblicke:

Es gibt viele Möglichkeiten, Zeichen zu löschen oder zu ersetzen, je nachdem, was das Ziel ist. Informationen darüber, wie diese Funktionen genau funktionieren und was sie tun, können in der entsprechenden Dokumentation gefunden werden. Es gibt auch andere Programmierkonzepte, wie Reguläre Ausdrücke, die ebenfalls für das Löschen von Zeichen verwendet werden können. Diese erfordern jedoch ein tieferes Verständnis der Programmierung und sind möglicherweise nicht die beste Wahl für Anfänger.

## Weitere Infos:

- [Dokumentation über die remove_if() Funktion](https://www.arduino.cc/reference/de/language/functions/string-manipulation/removeif/)
- [Dokumentation über die replace() Funktion](https://www.arduino.cc/reference/de/language/functions/string-manipulation/replace/)
- [Einblick in die Verwendung von Regulären Ausdrücken zur Zeichenlöschung](https://www.regular-expressions.info/)