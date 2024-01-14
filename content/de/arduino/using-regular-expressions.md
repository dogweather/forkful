---
title:    "Arduino: Verwendung von regulären Ausdrücken"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Arduino-Programmierung gibt es viele Möglichkeiten, um Daten zu verarbeiten. Eine sehr leistungsstarke und effiziente Methode ist die Verwendung von regulären Ausdrücken. Diese ermöglichen es uns, bestimmte Muster in Zeichenketten zu erkennen und zu manipulieren. Warum sollte man also reguläre Ausdrücke verwenden? Ganz einfach: Sie sparen Zeit und schreiben weniger Code. Mit weniger Code können Sie auch die Performance Ihrer Anwendung verbessern.

## Wie

Um reguläre Ausdrücke in Ihrem Arduino-Code zu verwenden, müssen Sie zunächst die Bibliothek <i>regex.h</i> in Ihr Projekt einbinden. Dann können Sie mit der Funktion <i>regex_match()</i> ein Muster in einer Zeichenkette suchen. Hier ist ein Beispiel, in dem wir prüfen, ob eine E-Mail-Adresse gültig ist:

```
#include <regex.h>

void setup() {
  Serial.begin(9600);
  String email = "beispiel@mail.de";
  
  if (regex_match(email, regex("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.(com|de|net)$"))) {
    Serial.println("Gültige E-Mail-Adresse");
  } else {
    Serial.println("Ungültige E-Mail-Adresse");
  }
}

void loop() {
  
}
```

Das Ergebnis dieser Code-Schnipsel wird "Gültige E-Mail-Adresse" sein, da das angegebene Muster mit der gegebenen E-Mail-Adresse übereinstimmt. Natürlich können Sie die regulären Ausdrücke auch für komplexere Aufgaben verwenden, wie beispielsweise das Extrahieren von Zahlen aus einer Zeichenkette.

## Deep Dive

Reguläre Ausdrücke können komplex werden, je nachdem welche Muster wir suchen möchten. Es gibt verschiedene Symbole und Operatoren, die uns dabei helfen können, unsere gewünschte Logik zu definieren. Zum Beispiel können wir Klammern verwenden, um Teile eines Musters zusammenzufassen und somit die Suche zu vereinfachen. Auch Quantifizierer wie <i>+</i> oder <i>*</i> können unseren regulären Ausdruck flexibler und leistungsfähiger machen.

Es gibt auch Möglichkeiten, unsere Ausdrücke zu optimieren, indem wir beispielsweise den Suchbereich mit der Funktion <i>regex_search()</i> einschränken oder mit der Option <i>icase</i> Groß- und Kleinschreibung ignorieren. Es gibt viele Ressourcen im Internet, die weitere Informationen und Tipps zu regulären Ausdrücken bieten.

## Siehe auch

- Die offizielle Dokumentation zu <i>regex.h</i>: https://www.arduino.cc/reference/en/language/functions/regex/
- Ein Tutorial zu regulären Ausdrücken auf arduino.cc: https://www.arduino.cc/reference/en/language/functions/regex/
- Ein interaktives Tool zum Testen von regulären Ausdrücken: https://regex101.com/