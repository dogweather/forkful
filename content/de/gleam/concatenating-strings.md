---
title:                "Zusammenführen von Zeichenketten"
html_title:           "Gleam: Zusammenführen von Zeichenketten"
simple_title:         "Zusammenführen von Zeichenketten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Verkettung von Zeichenfolgen ist eine häufig verwendete Technik in der Programmierung. Sie ermöglicht es, mehrere Textelemente miteinander zu verbinden, um eine längere Folge von Zeichen zu erstellen. Programmierer nutzen dies, um zum Beispiel Text für Benutzer auszugeben oder Strings für spezielle Berechnungen zu erstellen.

## Wie geht's?
Die Verkettung von Zeichenfolgen in Gleam ist einfach und unkompliziert. Ein Beispielcode könnte wie folgt aussehen:

```Gleam
fn double_greet(name1: String, name2: String) {
  // Verketten von Zeichenfolgen mit dem `++` Operator
  let greeting = "Hallo " ++ name1 ++ " und " ++ name2 ++ "!";
  // Ausgabe des Ergebnisses
  GreetingPrinter.print(greeting);
}

pub fn start() {
  double_greet("Anna", "Ben");
}
```

Die Ausgabe dieses Codes würde "Hallo Anna und Ben!" sein.

## Tiefere Einblicke
Die Idee der Zeichenfolgenverkettung wurde bereits in den 1960er Jahren mit dem Aufstieg der Programmiersprache COBOL populär. Heutzutage gibt es viele verschiedene Möglichkeiten, String-Verkettung in verschiedenen Programmiersprachen zu implementieren. Gleam verwendet den Operator `++`, um Strings zu verketten, aber es gibt auch andere Optionen wie zum Beispiel die `concat()` Funktion in Java. In der Gleam-Dokumentation werden weitere Details zur Implementierung der String-Verkettung erklärt.

## Sieh dir auch an
Wenn du mehr über Zeichenfolgenverkettung lernen möchtest, empfehlen wir dir die offizielle Gleam-Dokumentation unter https://gleam.run/. Dort findest du auch weitere spannende Informationen zu allen Aspekten der Programmierung mit Gleam.