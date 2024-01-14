---
title:                "Go: Das Lesen von Befehlszeilen-Argumenten"
simple_title:         "Das Lesen von Befehlszeilen-Argumenten"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein wichtiger Teil der Go-Programmierung. Wenn du wissen möchtest, wie du deine Anwendungen anpasst und erweiterst, dann ist das Verständnis von Befehlszeilenargumenten entscheidend.

## Wie man es macht

Das Lesen von Befehlszeilenargumenten in Go ist einfach und unkompliziert. Mit dem `flag` Paket kannst du ganz einfach Argumente aus der Befehlszeile in dein Programm einlesen.

```Go
import "flag"
```

Als nächstes musst du die Argumente definieren, die du lesen möchtest. Zum Beispiel könnten wir eine Variable `name` definieren, die den Namen des Benutzers aufnehmen soll.

```Go
var name string
```

Jetzt müssen wir nur noch das Argument mit dem `flag` Paket verknüpfen und das Programm ausführen.

```Go
flag.StringVar(&name, "name", "", "Gib deinen Namen ein.")
flag.Parse()
```

Du kannst nun den Wert von `name` in deinem Programm verwenden. Wenn wir beispielsweise den Befehl `go run main.go -name Max` ausführen, wird `name` auf den Wert "Max" gesetzt.

## Tiefentauchen

Das `flag` Paket bietet viele weitere Möglichkeiten, um Befehlszeilenargumente zu lesen und zu verarbeiten. Du kannst zum Beispiel auch verschiedene Datentypen wie Integers oder Booleans einlesen oder standardmäßige Werte für Argumente festlegen.

Weitere Informationen findest du in der offiziellen [Dokumentation des `flag` Pakets](https://golang.org/pkg/flag/).

## Siehe auch

- [Offizielle Go-Dokumentation](https://golang.org/doc/)
- [Go-Forum auf Reddit](https://www.reddit.com/r/golang/)
- [Go-Blog](https://blog.golang.org/)