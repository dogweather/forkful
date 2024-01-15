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

## Warum

Wenn du regelmäßig mit Texten arbeitest und immer wieder Aufgaben wie das Suchen und Ersetzen von bestimmten Wörtern oder Musters hast, dann sind reguläre Ausdrücke ein mächtiges Werkzeug, das dir viel Zeit und Mühe sparen kann. Mit Go, der aktuellen Version der Programmiersprache, kannst du reguläre Ausdrücke einfach und effizient in deinen Code integrieren.

## Wie es funktioniert

Reguläre Ausdrücke sind eine spezielle Syntax zum Suchen und Ersetzen von Texten, die auf einem bestimmten Muster basieren. Mit Go kannst du reguläre Ausdrücke innerhalb von "```Go ... ```" Code-Blöcken ganz einfach in deinem Code einbinden.

Hier ist ein Beispiel:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main(){
    text := "Hello there! Wie geht es dir?"

    // Regular expressions in Go sind case-sensitive 
    regex := regexp.MustCompile("wie|Wie")

    // Finde alle Vorkommen von "Wie" oder "wie" in dem Text
    matches := regex.FindAllString(text, -1)

    // Gib die Vorkommen aus
    fmt.Println(matches)
}
```

Die Ausgabe des oben genannten Codes wäre:

```
[Wie wie]
```

In diesem Beispiel haben wir ein einfaches reguläres Ausdrucksmuster erstellt, das nach allen Vorkommen von "Wie" oder "wie" in einem Text sucht. Mit regulären Ausdrücken kannst du aber auch komplexere Muster definieren, um noch spezifischer zu suchen.

## Tiefer eintauchen

Um reguläre Ausdrücke in Go effektiv zu nutzen, gibt es einige wichtige Dinge, die du wissen solltest:

- Reguläre Ausdrücke in Go sind immer case-sensitive, es sei denn, du verwendest den `i` Flag (z.B `/wie/i` würde auch "Wie" finden)
- Die Vollständigkeit eines regulären Ausdrucks kann mit den `^` (Anfang) und `$` (Ende) Zeichen angegeben werden. Zum Beispiel `/^wie$/` würde nur nach einem Wort suchen, das genau "wie" lautet, und keine Wörter wie "wieder" oder "leiwand" finden.
- Mit der `ReplaceAllString()` Funktion kannst du auch Teile eines Strings ersetzen, die einem bestimmten Muster entsprechen.
- Die `Match()` Funktion gibt `true` zurück, wenn ein Text zu dem regulären Ausdruck passt, anstatt es in ein Array zu speichern wie bei `FindAllString()`.
- Es gibt noch viele weitere Funktionen und Möglichkeiten beim Arbeiten mit regulären Ausdrücken, die du in der offiziellen [Go-Dokumentation](https://golang.org/pkg/regexp/) und [Beispielen](https://github.com/stoewer/go-strcase/blob/master/regression_test.go) finden kannst.

## Siehe auch

- [Einführung zu regulären Ausdrücken in Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-de)
- [Offizielle Go-Dokumentation zu regulären Ausdrücken](https://golang.org/pkg/regexp/)
- [10 Beispiele für reguläre Ausdrücke in Go](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions-go/)