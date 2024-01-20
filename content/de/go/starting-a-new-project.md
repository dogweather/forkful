---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt zu starten, bedeutet, von Null wegzugehen und zu beginnen, eine neue Software zu entwerfen und zu entwickeln. Programmierer tun dies, um Lösungen für spezifische Probleme zu schaffen oder neue Funktionen einzuführen.

## So geht's:

Ein neues Go-Projekt zu starten ist einfach. Wir erstellen ein neues Verzeichnis und initialisieren es als Go-Modul:

```Go
mkdir hello
cd hello
go mod init example.com/hello
```

Dann schreiben wir unseren ersten Code in einer Datei namens `main.go`:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hallo Welt")
}
```

Um unseren Code auszuführen, verwenden wir `go run`:

```Go
go run .
```

Die Ausgabe sollte "Hallo Welt" sein.

## Tiefgang:

### Historischer Hintergrund

Go, auch bekannt als Golang, wurde ab 2007 von Google entwickelt und 2009 veröffentlicht. Es wurde erstellt, um die Effizienz und Produktivität beim Schreiben von Software zu steigern, insbesondere im Zusammenhang mit großen Software-Systemen.

### Alternativen zu Go

Obwohl Go beliebt ist, gibt es auch andere Sprachen wie Python, Java und C#, die ebenfalls verwendet werden können, um neue Projekte zu starten. Die Wahl hängt meist von verschiedenen Faktoren wie Projektanforderungen, verfügbarem Fachwissen und persönlichen Vorlieben ab.

### Implementierungsdetails

In Go ist `main` die Funktion, bei der die Ausführung beginnt. `fmt.Println` wird verwendet, um auf den Standardausgabe-Stream zu drucken. `go mod init` initialisiert das aktuelle Verzeichnis als Go-Modul, wodurch wir Pakete in unserem Projekt ordnen und Versionskontrolle hinzufügen können.

## Weiterführende Ressourcen:

- Go-Webseite für Tutorials und Dokumentationen: https://golang.org/
- Go-Community: https://gophers.slack.com/
- Go-Playground zum Testen und Teilen von Go-Code: https://play.golang.org/