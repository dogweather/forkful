---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Go: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind ein nützliches Werkzeug in der Programmierung, mit dem man Texte auf bestimmte Muster überprüfen und bearbeiten kann. Programmierer nutzen reguläre Ausdrücke, um Textdateien zu durchsuchen, Eingaben von Nutzern zu validieren oder Daten aus komplexen Dateiformaten zu extrahieren.

## How to:
In Go können reguläre Ausdrücke mit dem Paket "regexp" verwendet werden. Zunächst muss das Paket importiert werden:
```
import "regexp"
```
Dann kann man ein reguläres Ausdrucksobjekt erstellen, indem man den gewünschten Ausdruck und die zu untersuchende Zeichenkette übergeben:
```
re := regexp.MustCompile("ab+c")
str := "abbc"
```
Um zu überprüfen, ob die Zeichenkette dem regulären Ausdruck entspricht, kann man die "MatchString" Funktion verwenden:
```
matches := re.MatchString(str)
fmt.Println(matches) // true
```
Man kann auch den regulären Ausdruck direkt in die MatchString Funktion schreiben:
```
matches := regexp.MatchString("ab+c", "abbbc")
fmt.Println(matches) // true
```
Um Teile der gefundenen Muster zu extrahieren, kann man die "FindString" Funktion verwenden:
```
result := re.FindString(str)
fmt.Println(result) // abbc
```

## Deep Dive:
Reguläre Ausdrücke wurden ursprünglich in den 1950er Jahren von dem Mathematiker Stephen Kleene entwickelt. Sie wurden in den 1960er Jahren in verschiedenen Programmiersprachen implementiert und sind seitdem ein wichtiger Bestandteil der Programmierung. Es gibt auch alternative Methoden wie String-Suchalgorithmen, aber reguläre Ausdrücke bieten eine kompaktere und einfacher zu lesende Möglichkeit, Muster in Texten zu finden und zu manipulieren.

Die Implementierung von regulären Ausdrücken in Go basiert auf der PCRE (Perl Compatible Regular Expression) Bibliothek. Go bietet jedoch zusätzliche Funktionen und Erweiterungen, wie zum Beispiel die "MatchString" Funktion, die es einfacher macht, reguläre Ausdrücke in Go zu verwenden als in anderen Sprachen.

## See Also:
- [Golang 'regexp' Paket Dokumentation](https://golang.org/pkg/regexp/)