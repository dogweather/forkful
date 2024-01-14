---
title:    "Go: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Immer wieder kommt es vor, dass man in der Programmierung mit Strings arbeiten muss. Eine gängige Anforderung ist dabei oft das Umwandeln eines Strings in Kleinbuchstaben. Warum sollte man das tun und wie funktioniert es in Go? In diesem Blog-Beitrag werden wir uns genau damit beschäftigen.

## Anleitung

Die Go-Standardbibliothek bietet eine einfache Funktion, um einen String in Kleinbuchstaben zu konvertieren. Diese heißt `ToLower` und kann auf einen String angewendet werden, der in unserem Fall einfach eine Variable oder ein String-Literal sein kann.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "GOLANG"
    lowerStr := strings.ToLower(str)
    fmt.Println(lowerStr)
}
```

Die Ausgabe dieses Codes wäre `golang`, da die Funktion `ToLower` alle Zeichen des Strings in Kleinbuchstaben umwandelt.

## Tiefere Einblicke

Wenn man genauer betrachtet, was genau in der `ToLower`-Funktion passiert, erfährt man, dass sie sich zunächst den Zeichensatz des Betriebssystems anschaut. Da dieser in der Regel Großbuchstaben verwendet, wie zum Beispiel `ABCDEFGHIJKLMNOPQRSTUVWXYZ`, weiß die Funktion, welche Zeichen in Kleinbuchstaben umgewandelt werden müssen. Anschließend wird eine neue `Rune`-Instanz erstellt, die alle Zeichen des Strings in Kleinbuchstaben enthält. Zuletzt wird diese Instanz zu einem String konvertiert und ausgegeben.

Man sollte jedoch beachten, dass die `ToLower`-Funktion nur ASCII-Zeichen in Kleinbuchstaben umwandeln kann. Sollten im String also nicht-ASCII-Zeichen enthalten sein, müssen diese manuell bearbeitet werden.

## Siehe auch

* [Go-Standardbibliothek: `ToLower`-Funktion](https://golang.org/pkg/strings/#ToLower)
* [Blog-Beitrag über Strings in Go](https://www.digitalocean.com/community/tutorials/string-manipulation-in-go-de#getting-started-with-strings)
* [Beispielcode für das Konvertieren von Strings in Go](https://gobyexample.com/string-functions)