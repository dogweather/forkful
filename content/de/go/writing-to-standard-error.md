---
title:                "Go: Schreiben in Standardfehler"
simple_title:         "Schreiben in Standardfehler"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Schreiben Sie an die Standardfehlerausgabe kann sehr hilfreich sein, um Fehlermeldungen und andere wichtige Informationen während der Ausführung eines Go-Programms zu erhalten. Auf diese Weise können Entwickler Probleme schnell erkennen und beheben, um sicherzustellen, dass ihre Programme reibungslos funktionieren.

# Wie geht das?

Um an die Standardfehlerausgabe in Go zu schreiben, verwenden Sie einfach die "fmt.Fprintln" Funktion und geben Sie "os.Stderr" als ersten Parameter an. Hier ist ein Beispielcode, der "Hello World" an die Standardfehlerausgabe schreibt:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Fprintln(os.Stderr, "Hello World")
}
```

Die Ausgabe dieses Programms wäre:

```
Hello World
```

# Tiefergehende Information

In Go gibt es auch die Möglichkeit, direkt auf den "stderr" Stream zuzugreifen, indem man die "os.Stderr" Variable verwendet. Dies kann nützlich sein, um andere Informationen außerhalb von "fmt.Fprintln" an die Standardfehlerausgabe zu schreiben. Hier ist ein Beispielcode:

```Go
package main

import (
	"os"
)

func main() {
	os.Stderr.WriteString("Dies ist eine weitere Nachricht an die Standardfehlerausgabe.")
}
```

Die Ausgabe dieses Programms wäre:

```
Dies ist eine weitere Nachricht an die Standardfehlerausgabe.
```

# Siehe auch

Hier sind einige nützliche Ressourcen, die Ihnen helfen können, mehr über das Schreiben an die Standardfehlerausgabe in Go zu erfahren:

- [Go-Dokumentation über die "fmt" Paket](https://golang.org/pkg/fmt)
- [Offizielle Go-Website](https://golang.org)
- [Go-Community-Forum](https://forum.golangbridge.org)