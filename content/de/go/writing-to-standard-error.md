---
title:                "Schreiben auf den Standardfehler"
html_title:           "Go: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben auf den Standardfehler ist eine Möglichkeit für Programmierer, Fehlermeldungen oder andere wichtige Informationen auf dem Terminal auszugeben. Dies ist besonders nützlich, wenn das Programm im Hintergrund ausgeführt wird und es keine grafische Benutzeroberfläche gibt. Ebenfalls hilfreich ist diese Methode, um sicherzustellen, dass wichtige Informationen nicht in der Standardausgabe überschrieben werden.

## Wie geht's?
Um auf den Standardfehler zu schreiben, kann die Funktion ```Fprintf``` aus der Standardbibliothek "fmt" verwendet werden. Ein einfaches Beispiel dafür sieht folgendermaßen aus:

```Go
package main

import "fmt"

func main() {
    fmt.Fprintf(os.Stderr, "Dies ist eine Fehlermeldung")
}
```

Die Ausgabe wird dann auf dem Terminal als "Dies ist eine Fehlermeldung" angezeigt. Wenn Sie in Ihrer Funktion bereits einen Fehlerwert haben, können Sie auch die Funktion ```Fprintln``` verwenden, um eine Fehlermeldung zu generieren. Ein solches Beispiel sieht wie folgt aus:

```Go
package main

import "fmt"

func main() {
    if err := doSomething(); err != nil {
        fmt.Fprintln(os.Stderr, "Fehler:", err)
    }
}
```

Die Ausgabe würde dann beispielsweise "Fehler: Datei nicht gefunden" sein.

## Tiefer tauchen
Das Schreiben auf den Standardfehler ist eine gängige Praxis in der Programmierung und wird auch von vielen anderen Sprachen unterstützt. Beispiele dafür sind die Funktionen "console.error" in JavaScript oder "System.err" in Java.

Alternativ zu der Funktion ```Fprintf``` kann auch direkt auf die Variable ```os.Stderr``` zugegriffen werden, um Text auf den Standardfehler zu schreiben.

In der Go-Standardbibliothek gibt es auch die Funktion ```Log``` aus dem Paket "log", die automatisch auf den Standardfehler schreibt. Diese kann jedoch nicht so präzise angepasst werden wie die Funktionen aus dem Paket "fmt".

## Siehe auch
[Die offizielle Dokumentation zu "fmt.Fprintf"](https://golang.org/pkg/fmt/)