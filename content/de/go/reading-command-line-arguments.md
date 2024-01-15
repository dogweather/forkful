---
title:                "Das Lesen von Befehlszeilenargumenten"
html_title:           "Go: Das Lesen von Befehlszeilenargumenten"
simple_title:         "Das Lesen von Befehlszeilenargumenten"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie Go programmieren, werden Sie früher oder später auf die Anforderung stoßen, Befehlszeilenargumente zu lesen. Dies ist besonders nützlich, wenn Sie Ihre Programme anpassungsfähiger machen möchten, da Benutzer verschiedene Einstellungen durch die Befehlszeile übergeben können.

## Wie gehts

Um Befehlszeilenargumente in Go zu lesen, können Sie die `os.Args` Funktion verwenden. Diese gibt eine `[]string` zurück, in der jedes Element ein Befehlszeilenargument darstellt. Beispiel:

```Go
func main() {
    args := os.Args
    fmt.Println(args)
}
```

Wenn Sie nun Ihr Programm mit dem Befehlszeilenargument `foo bar` aufrufen, wird die Ausgabe `["meinprogramm" "foo" "bar"]` sein.

Sie können auch auf bestimmte Argumente in der `[]string` zugreifen, indem Sie den Index verwenden. Beispiel:

```Go
func main() {
    args := os.Args
    fmt.Println(args[0]) // gibt "meinprogramm" aus
    fmt.Println(args[1]) // gibt "foo" aus
}
```

Dies ermöglicht es Ihnen, auf spezifische Argumente in Ihrem Programm zuzugreifen und diese zur Laufzeit zu verwenden.

## Tiefere Einblicke

Sie können auch die `flag`-Paket in Go verwenden, um Befehlszeilenargumente zu lesen. Dies ermöglicht es Ihnen, auch Argumente mit verschiedenen Flags zu übergeben und diese bequem beim Ausführen Ihres Programms zu überprüfen. Beispiel:

```Go
func main() {
    name := flag.String("name", "Gopher", "Name of user")
    age := flag.Int("age", 25, "Age of user")
    flag.Parse()
    fmt.Printf("Hello %s, you are %d years old!", *name, *age)
}
```

Wenn Sie nun Ihr Programm mit dem Befehlszeilenargument `--name Alice --age 30` aufrufen, wird die Ausgabe `Hello Alice, you are 30 years old!` sein.

Sie können auch benutzerdefinierte Flags und Argumente erstellen, indem Sie die `flag`-Paket weiter erkunden. Dies kann nützlich sein, wenn Sie ein Programm mit vielen verschiedenen Einstellungen und Optionen haben.

## Siehe auch

- [Die offizielle Dokumentation zu os.Args](https://golang.org/pkg/os/#Args)
- [Die offizielle Dokumentation zum flag-Paket](https://golang.org/pkg/flag/)