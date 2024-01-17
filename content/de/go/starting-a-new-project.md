---
title:                "Ein neues Projekt starten"
html_title:           "Go: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Erstellen eines neuen Projekts ist der erste Schritt, um eine Softwarelösung mit Go zu entwickeln. Programmierer starten neue Projekte, um eine bestimmte Aufgabe oder Funktion zu lösen und/oder um bestehenden Code effizienter zu gestalten.

# Wie gehts?

Um ein neues Projekt in Go zu starten, folgen Sie einfach diesen Schritten:

1. Öffnen Sie Ihr bevorzugtes Textbearbeitungsprogramm.
2. Erstellen Sie eine neue Datei mit der Endung `.go`.
3. Schreiben Sie Ihren Code in dieser Datei und speichern Sie sie ab.
4. Öffnen Sie die Kommandozeile und navigieren Sie zum Speicherort der Datei.
5. Geben Sie den Befehl `go run [Dateiname].go` ein, um Ihr Programm auszuführen.

Eine einfache "Hello World"-Anwendung in Go könnte beispielsweise folgendermaßen aussehen:

```
package main

import "fmt"

func main() {
	fmt.Println("Hallo Welt!")
}
```

Die Ausgabe wäre dann `Hallo Welt!`.

# Tiefergehende Informationen

Go wurde 2009 von Google entwickelt und ist eine Open-Source-Programmiersprache. Es wurde entworfen, um einfache aber leistungsstarke Softwarelösungen zu erstellen. Eine Alternative zu Go ist beispielsweise die Programmiersprache Python. Der Vorteil von Go gegenüber anderen Sprachen ist, dass es kompiliert wird, was bedeutet, dass der Code zu ausführbarem Maschinencode übersetzt wird und somit in der Regel schneller läuft.

# Siehe auch

Weitere Informationen und Tutorials zu Go finden Sie auf der offiziellen Website: https://golang.org/

Für eine interaktive Einführung in Go können Sie auch das Go-Tutorial unter https://tour.golang.org/ ausprobieren.