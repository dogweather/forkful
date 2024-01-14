---
title:    "Arduino: Verwendung von regulären Ausdrücken"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug, um Textmuster in Programmierprojekten zu erkennen und zu manipulieren. Sie können dabei helfen, komplexe Aufgaben wie Datenvalidierung, Suchen und Ersetzen von Text und die Aufteilung von Zeichenketten zu lösen. Für alle, die programmieren, sind reguläre Ausdrücke ein wichtiges Konzept, das das Schreiben effizienter und robusterer Programme ermöglicht.

## Wie geht es

Um reguläre Ausdrücke in Ihrem Arduino-Code zu verwenden, müssen Sie die Bibliothek "Regex" einbinden. Dann können Sie reguläre Ausdrücke mit der Funktion "match()" anwenden. Schauen wir uns ein Beispiel an, in dem wir eine Telefonnummer aus einer Zeichenkette extrahieren:

```Arduino
#include <Regex.h>

String text = "+49 1234 567890";
Regex phonePattern("([+])(\\d{2})(\\s)(\\d{4})(\\s)(\\d{6})");

if (text.match(phonePattern)) {
  String extractedPhone = phonePattern.matched(0); // Extrahiert die gesamte Telefonnummer
  String countryCode = phonePattern.matched(1); // Extrahiert die Ländervorwahl
  String areaCode = phonePattern.matched(2); // Extrahiert die Ortsvorwahl
  String localNumber = phonePattern.matched(3); // Extrahiert die lokale Nummer
}
```

In diesem Beispiel sehen wir, dass wir zuerst die Bibliothek "Regex" einbinden und dann eine Zeichenkette definieren, in der wir eine Telefonnummer suchen möchten. Mit Hilfe des regulären Ausdrucks können wir die Telefonnummer in mehrere Teile aufteilen und die einzelnen Komponenten extrahieren. Dies kann besonders nützlich sein, wenn Sie Daten aus verschiedenen Quellen in Ihrem Code verarbeiten müssen. Mit regulären Ausdrücken können Sie auch verschiedene Bedingungen festlegen, um sicherzustellen, dass die gesuchten Muster genau übereinstimmen.

## Tiefer Einblick

Während reguläre Ausdrücke sehr nützlich sind, gibt es einige Punkte, die Sie bei der Verwendung von regulären Ausdrücken in Ihrem Arduino-Code beachten sollten. Erstens benötigen reguläre Ausdrücke mehr Ressourcen als herkömmliche String-Manipulationstechniken. Wenn Sie also in einem speicherbegrenzten Projekt arbeiten, sollten Sie die Verwendung von regulären Ausdrücken sorgfältig abwägen. Zweitens müssen Sie bei der Verwendung von Variablen in Ihrem regulären Ausdruck auf die richtige Datentypzuweisung achten, da dies die Leistung beeinflussen kann. Schließlich sollten Sie, wenn Sie regelmäßig komplexe oder anspruchsvolle Muster erkennen müssen, erwägen, einen speziellen regulären Ausdruck-Parser zu verwenden, der in der Regel eine bessere Leistung bietet.

## Siehe auch

* [Offizielle Dokumentation zur Regex-Bibliothek für Arduino](https://github.com/arduino-libraries/Regex)
* [Regex-Tutorial für Anfänger](https://www.regular-expressions.info/tutorial.html)
* [Alternativen zur Verwendung regulärer Ausdrücke in Arduino](https://arduino.stackexchange.com/questions/2996/alternative-to-using-string-regex-match-in-arduino)