---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Suche und Ersetzung von Text ist eine gängige Aktion, die Zeichenketten findet und durch neue ersetzt. Programmierer verwenden sie oft, um Daten zu manipulieren und Code zu bereinigen.

## So geht's:
In Go können Sie die Funktion `strings.Replace()` zum Suchen und Ersetzen von Text verwenden. Hier ist ein einfaches Beispiel:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Hallo Welt"
    newstr := strings.Replace(str, "Welt", "Go", -1)

    fmt.Println(newstr)
}
```
Die Ausgabe wird "Hallo Go" sein.

## Tiefgreifende Details
Die Suche und Ersetzung von Text ist ein wichtiger Teil der textverarbeitenden Programmiersprachen und wurde erstmals in der Sprache "sed" gründlich genutzt. In Go ersetzt die Funktion `strings.Replace()` erstmals alle Vorkommen eines bestimmten Teilstrings. Die "-1" als letzter Parameter bedeutet, dass alle Instanzen ersetzt werden sollen. Alternativ können Sie eine bestimmte Anzahl an Vorkommen ersetzen, indem Sie diese Zahl anstelle von "-1" verwenden.

Die `strings.Replace()` Methode ist praktisch, aber nicht die einzige Option in Go. Sie können reguläre Ausdrücke mit dem `regexp` Paket verwenden, wenn Sie mehr Flexibilität benötigen.

## Siehe Auch
Hier sind einige zusätzliche Ressourcen, die Ihnen weitere Informationen liefern:

1. Go Dokumentation zur Strings Library: https://golang.org/pkg/strings/
2. Go Dokumentation zum Regexp Paket: https://golang.org/pkg/regexp/
3. Einführung in die Arbeit mit Text in Go: https://www.ardanlabs.com/blog/2017/08/working-with-text-in-go.html
4. Tutorial zur Manipulation von Strings in Go: https://gobyexample.com/string-manipulation