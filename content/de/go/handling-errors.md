---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:53:19.970564-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?

Fehlerbehandlung in Go dreht sich darum, Laufzeitprobleme anmutig aufzufangen und darauf zu reagieren. Wir tun dies, um Abstürze zu verhindern und sicherzustellen, dass unsere Programme vorhersehbar handeln, auch wenn Probleme auftreten.

## Wie geht das:

Go verwendet explizite Fehlerbehandlung. Das bedeutet, dass du überprüfst, ob eine Funktion bei jedem Aufruf einen Fehler zurückgibt. Keine Ausnahmen. So sieht das aus:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Uh oh:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// So tun, als ob etwas schief gelaufen wäre
	return fmt.Errorf("etwas ist schief gelaufen")
}
```

Führe dies aus, und du bekommst:

```
Uh oh: etwas ist schief gelaufen
```

Aber was, wenn es erfolgreich ist?

```Go
func doSomething() error {
	// Dieses Mal ist alles gut
	return nil
}
```

Keine Ausgabe. Cool, keine Nachrichten sind gute Nachrichten.

## Tiefere Betrachtung:

In Go war die Fehlerbehandlung schon immer ein Streitpunkt. Seit dem Anfang hat sich Go gegen Ausnahmen entschieden zugunsten eines expliziteren Ansatzes, den einige Entwickler für seine Einfachheit lieben und andere als weitschweifig empfinden. Der eingebaute `error`-Typ ist ein Interface. Jeder Typ mit einer `Error() string`-Methode erfüllt es. Dies passt zum Ethos von Go mit Einfachheit und Explizitheit.

Alternativen? Es gibt das Duo `panic` und `recover`, aber diese sind für außergewöhnliche Fälle gedacht (Wortspiel beabsichtigt), wenn das Programm nicht fortgesetzt werden kann. Denke an `panic` als den Schleudersitz, den du betätigst, wenn du weißt, dass es kein Zurück gibt. Benutze es sparsam.

Was die gängige Fehlerbehandlung angeht, führte Go 1.13 das Fehlerverpacken ein, wodurch es einfacher wird, die "Fehlerkette" mit Funktionen wie `errors.Is()` und `errors.As()` herauszufinden.

## Siehe auch:

Alles über Fehlerbehandlung in Go:

- Der Go Blog über Fehlerbehandlung: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – Abschnitt zur Fehlerbehandlung: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 Dokumentation zum Fehlerverpacken: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheneys Beitrag zu Strategien für Fehlerbehandlung: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
