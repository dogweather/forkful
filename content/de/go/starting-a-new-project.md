---
title:                "Einen neuen Projekt starten"
date:                  2024-01-20T18:03:29.435905-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen neuen Projekt starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt zu starten bedeutet, eine leere Leinwand in eine funktionierende Software zu verwandeln. Programmierer machen das, um Ideen zum Leben zu erwecken, Probleme zu lösen oder einfach um neue Fertigkeiten zu lernen.

## Anleitung:

Um mit Go ein neues Projekt anzufangen, installiere erst Go von [golang.org](https://golang.org/dl/). Dann:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hallo, neues Projekt!")
}
```

Speichere dies in einer Datei namens `main.go` und führe es aus mit:

```bash
go run main.go
```

Erwartete Ausgabe:

```
Hallo, neues Projekt!
```

Um das Projekt zu bauen (kompilieren):

```bash
go build main.go
```

Dies erzeugt eine ausführbare Datei in deinem aktuellen Verzeichnis.

## Vertiefung:

Go ist dafür bekannt, die Entwicklung von zuverlässigen und einfach zu wartenden Programmen zu erleichtern. Seit seiner Einführung von Google im Jahr 2009 wurde Go oft als Werkzeug für Systemprogrammierung und Web-Backends verwendet. Alternativen wie Python, Java, oder Rust haben auch ihre Stärken, aber Go besticht durch seine einfache Handhabung von Concurrent Programming und seine starke und standardisierte Toolchain.

Wenn du ein neues Go-Projekt anfängst, denke über die Struktur nach. Verwende `go mod init` um ein modulares Projekt zu initialisieren, das wird deine Abhängigkeiten verwalten. Dies ist seit der Version 1.11 Bestandteil von Go und löste ältere Systeme wie `GOPATH` ab.

Ein simples Beispiel, um ein Modul zu initialisieren:

```bash
go mod init meinprojekt
```

Daraufhin wird eine `go.mod` Datei in deinem Projektverzeichnis erstellt. Diese Datei hält deine Abhängigkeiten fest.

## Siehe Auch:

- Die offizielle Go Webseite [golang.org](https://golang.org/)
- Go's "Getting Started" Guide: [golang.org/doc/install](https://golang.org/doc/install)
- Das Go Blog für aktuelle Nachrichten und Updates: [blog.golang.org](https://blog.golang.org/)
- [Go by Example](https://gobyexample.com/), eine hervorragende Ressource für praktische Beispiele
