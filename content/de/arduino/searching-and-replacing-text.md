---
title:                "Suchen und Ersetzen von Text"
date:                  2024-01-20T17:57:10.953758-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Text suchen und ersetzen bedeutet, bestimmte Zeichenfolgen in einem Text zu finden und sie durch andere zu ersetzen. Programmierer nutzen es, um Fehler zu beheben, Daten zu aktualisieren oder Code schnell anzupassen.

## How to:
Hier ist ein einfacher Code, der zeigt, wie man Text auf einem Arduino sucht und ersetzt. Wir nehmen an, dass `inputString` der Text ist, den wir bearbeiten wollen und `oldWord` das Wort, das wir ersetzen möchten.

```arduino
String inputString = "Ich mag Arduinos. Arduinos sind spaßig.";
String oldWord = "Arduinos";
String newWord = "Mikrocontroller";

void setup() {
  Serial.begin(9600);
  String replacedString = inputString;
  replacedString.replace(oldWord, newWord);
  Serial.println(replacedString);
}

void loop() {
  // Nichts benötigt für dieses Beispiel
}
```
Beim Ausführen dieses Codes wird im Serial Monitor das Ergebnis angezeigt:
```
Ich mag Mikrocontroller. Mikrocontroller sind spaßig.
```

## Deep Dive
Suchen und Ersetzen gibt es seit den frühen Tagen der Textverarbeitung; es ist ein Grundbestandteil der meisten Editoren. In der Arduino-Welt ist es zwar weniger häufig, aber immer noch nützlich für Dinge wie die Formatierung von Strings für Displays oder die Anpassung von Daten für die Weiterverarbeitung.

Alternativ kann man reguläre Ausdrücke (regex) in Sprachen wie Python oder JavaScript verwenden, diese sind in Arduino allerdings nicht nativ unterstützt und benötigen eine Bibliothek.

Die `replace`-Methode in Arduino erstellt keine neue Kopie des Strings, sondern ändert den originalen String direkt. In Sachen Speichernutzung ist das effizient, bedeutet aber auch, dass die Originaldaten verloren gehen.

## See Also
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Arduino Serial Communication](https://www.arduino.cc/reference/en/language/functions/communication/serial/) 
- [C++ std::string::replace](http://www.cplusplus.com/reference/string/string/replace/) für einen tieferen Einblick, wie diese Funktionalität in C++ implementiert ist.
