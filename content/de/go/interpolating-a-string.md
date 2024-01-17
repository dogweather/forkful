---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Go: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Interpolieren einer Zeichenfolge füllt der Programmierer einen Platzhalter in einer Zeichenfolge mit dynamischen Werten aus. Dadurch kann der Code effizienter und flexibler gestaltet werden.

## Wie geht's:
Beispielcode für die Verwendung von Interpolation in Go:

```
name := "Jane"
greeting := fmt.Sprintf("Hallo %s, wie geht es dir?", name)
fmt.Println(greeting)
```
Ausgabe: Hallo Jane, wie geht es dir?

```
age := 25
message := fmt.Sprintf("Ich bin %d Jahre alt.", age)
fmt.Println(message)
```
Ausgabe: Ich bin 25 Jahre alt.

## Tiefer tauchen:
Historischer Kontext: Die Idee des Interpolierens stammt aus dem Bereich der Textverarbeitung, wo Platzhalter mit tatsächlichen Werten aus einer Datenbank gefüllt wurden. In der Programmierung wird es häufig verwendet, um SQL-Abfragen dynamisch zu gestalten.

Alternativen: In Go gibt es mehrere Möglichkeiten, um Strings zu interpolieren. Eine Alternative ist die Verwendung eines Templates, das spezielle Markierungssymbole verwendet, um Platzhalter zu kennzeichnen. Eine andere Möglichkeit ist die Verwendung von String-Konkatenation, bei der Werte manuell an eine Zeichenfolge angehängt werden.

Implementierungsdetails: In Go wird die Funktion "fmt.Sprintf" verwendet, um einen formatierten String zurückzugeben. Diese Funktion unterstützt verschiedene Formatierungsoperationen wie %s für Zeichenfolgen, %d für ganze Zahlen und %f für Dezimalzahlen. Mehr Informationen dazu können in der offiziellen Dokumentation zu finden sein.

## Siehe auch:
Weitere Informationen zu String-Interpolation in Go finden Sie in der offiziellen Dokumentation unter https://golang.org/pkg/fmt/ und https://gobyexample.com/string-formatting.