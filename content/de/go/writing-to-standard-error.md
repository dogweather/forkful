---
title:    "Go: Zum Standardfehler schreiben"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

In der Go-Programmierung ist es oft nötig, Informationen oder Fehlermeldungen auszugeben. Eine gängige Methode dafür ist das Schreiben in die Standardfehlerausgabe (standard error). In diesem Blogbeitrag erfahrt ihr, warum und wie man in Go nach standard error schreibt.

## Wie geht man vor?

Um Inhalt an die Standardfehlerausgabe zu schreiben, kann die Funktion `fmt.Fprintln()` verwendet werden, die einen Text und eine io.Writer-Schnittstelle erwartet. Standardmäßig wird die Standardfehlerausgabe jedoch automatisch verwendet, daher reicht es in der Regel aus, `fmt.Println()` zu benutzen. Hier ein Beispiel:

```Go
// Ein einfaches Beispiel, um eine Fehlermeldung auszugeben
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Println("Dies ist eine Fehlermeldung")
    os.Exit(1) // Beendet das Programm mit Exit-Code 1
}
```

Die Ausgabe des obigen Beispiels wird auf der Konsole wie folgt aussehen:

```
Dies ist eine Fehlermeldung
```

## Tiefergehende Informationen

In Go gibt es weitere Möglichkeiten, um an die Standardfehlerausgabe zu schreiben, wie zum Beispiel die Funktion `os.Stderr.Write()`, die eine Byte-Array und eine io.Writer-Schnittstelle erwartet. Diese Funktion gibt die Bytes direkt an die Standardfehlerausgabe weiter. Hier ein Beispiel:

```Go
// Ein Beispiel, um ein Byte-Array an die Standardfehlerausgabe zu schreiben
package main

import (
    "fmt"
    "os"
)

func main() {
    errMsg := []byte("Dies ist eine Fehlermeldung")
    os.Stderr.Write(errMsg)
    os.Exit(1) // Beendet das Programm mit Exit-Code 1
}
```

Die Ausgabe wird auch hier wieder die gleiche sein wie oben gezeigt.

## Siehe auch

- [Go Doc: fmt.Fprintln()](https://golang.org/pkg/fmt/#Fprintln)
- [Go Doc: os.Exit()](https://golang.org/pkg/os/#Exit)
- [Go Doc: os.Stderr.Write()](https://golang.org/pkg/os/#Stderr.Write)