---
title:    "Arduino: Suchen und Ersetzen von Text"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Bei der Programmierung mit Arduino kann es oft vorkommen, dass man Text in seinem Code ersetzen muss. Mögliche Gründe dafür können beispielsweise Programmierfehler, Änderungen an Variablennamen oder das Anpassen von Funktionsaufrufen sein. In diesem Blogbeitrag werden wir uns ansehen, wie man innerhalb des Arduino-Codes effektiv Text suchen und ersetzen kann.

## Wie geht das?

Das Suchen und Ersetzen von Text innerhalb des Arduino-Codes kann durch den Einsatz der `replace()` Funktion erreicht werden. Diese Funktion erwartet mindestens drei Parameter: Der zu durchsuchende Text, der Text, der an dessen Stelle eingefügt werden soll, und der String, in dem die Suche und Ersetzung durchgeführt werden soll. Ein Beispielcode könnte folgendermaßen aussehen:

```Arduino

// Beispielcode für die Verwendung der replace() Funktion
String text = "Hallo Welt";
String ersetzung = "Guten Morgen";
text.replace("Welt", ersetzung);
Serial.println(text); // Ausgabe: "Hallo Guten Morgen"
```

In diesem Beispiel wird der Suchtext "Welt" durch den Text "Guten Morgen" ersetzt. Die `replace()` Funktion kann jedoch nicht nur einzelne Zeichen oder Wörter, sondern auch ganze Sätze oder sogar reguläre Ausdrücke ersetzen.

## Tiefere Einblicke

Um die `replace()` Funktion effektiv zu nutzen, ist es wichtig zu wissen, dass sie immer nur die erste Übereinstimmung im String ersetzt. Möchte man alle Übereinstimmungen ersetzen, muss man die Funktion innerhalb einer Schleife aufrufen. Außerdem kann die Funktion auch dazu verwendet werden, Text in anderen Datentypen wie beispielsweise Integer oder Float umzuwandeln. Hierfür wird der entsprechende Wert als String übergeben, in dem die Suche und Ersetzung durchgeführt wird. 

Neben der `replace()` Funktion gibt es auch noch andere Möglichkeiten, Text innerhalb des Arduino-Codes zu suchen und zu ersetzen. Eine Alternative ist beispielsweise die Verwendung der `indexOf()` und `substring()` Funktionen. Mit `indexOf()` kann man die Position eines bestimmten Textes im String ermitteln und mit `substring()` kann man einen Teil des Strings auslesen oder ersetzen. 

## Siehe auch

- [Offizielle Arduino Referenz für die replace() Funktion](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Tutorial: Text suchen und ersetzen mit Arduino](https://www.youtube.com/watch?v=804E2 JUtug0)
- [Tutorial: Textmanipulation mit String-Methoden in Arduino](https://www.youtube.com/watch?v=4ylA5C8Mo48)