---
title:    "Go: Das Lesen von Befehlszeilen-Argumenten"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein fundamental wichtiger Teil des Programmierens in Go, da es ermöglicht, Benutzereingaben zu verarbeiten und die Ausführung von Programmen zu steuern. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man Befehlszeilenargumente in Go liest und verarbeitet.

## Wie geht das?

Um Befehlszeilenargumente in Go zu lesen, verwenden wir die eingebaute `os.Args` Funktion. Diese Funktion gibt ein Array von Strings zurück, das alle Befehlszeilenargumente enthält, die beim Ausführen des Programms angegeben wurden.

```Go
func main() {

    // Lesen der Befehlszeilenargumente
    args := os.Args

    // Ausgabe des ersten Arguments
    fmt.Println("Erstes Argument:", args[0])

    // Ausgabe aller Argumente
    fmt.Println("Alle Argumente:", args)
}
```

Wenn wir dieses Programm mit dem Befehl `go run main.go hello world` ausführen, wird folgende Ausgabe erzeugt:

```
Erstes Argument: main.go
Alle Argumente: [main.go hello world]
```

Wie man sehen kann, wird das erste Argument (der Dateiname) automatisch hinzugefügt, daher beginnt das Array bei Index 0. Die restlichen Befehlszeilenargumente folgen in der Reihenfolge, in der sie angegeben wurden.

## Tiefer tauchen

Neben der Verwendung der `os.Args` Funktion gibt es noch andere Möglichkeiten, Befehlszeilenargumente in Go zu lesen und zu verarbeiten. Eine davon ist die Verwendung des `flag` Paketes, das eine bequemere Art bietet, Befehlszeilenargumente zu definieren und zu lesen.

```Go
// Importieren des "flag" Pakets
import "flag"

func main() {

    // Definieren von Befehlszeilenargumenten
    name := flag.String("name", "", "Name des Benutzers")
    age := flag.Int("age", 0, "Alter des Benutzers")

    // Ausführen der Parsing-Funktion
    flag.Parse()

    // Ausgabe der Argumente
    fmt.Println("Name:", *name)
    fmt.Println("Alter:", *age)
}
```

Wenn wir dieses Programm mit dem Befehl `go run main.go -name Max -age 25` ausführen, wird folgende Ausgabe erzeugt:

```
Name: Max
Alter: 25
```

Das `flag` Paket bietet auch die Möglichkeit, Argumente als boolesche Werte oder Listen von Strings zu definieren und zu verarbeiten.

## Siehe auch

Für weitere Informationen und Beispiele zur Verwendung von Befehlszeilenargumenten in Go empfehle ich folgende Links:

- [Offizielle Dokumentation zu os.Args](https://golang.org/pkg/os/)
- [Offizielle Dokumentation zum flag Paket](https://golang.org/pkg/flag/)
- [Ein Tutorial zu Befehlszeilenargumenten in Go](https://gobyexample.com/command-line-arguments)
- [Ein praktisches Beispiel zur Verwendung von flag in Go](https://golangbot.com/command-line-arguments/)