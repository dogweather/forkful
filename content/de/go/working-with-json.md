---
title:                "Go: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-json.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man mit JSON in Go arbeiten möchte. JSON ist ein sehr verbreitetes Datenformat, das oft in der Web-Entwicklung verwendet wird. Es ist einfach zu lesen und zu schreiben, und Go bietet eine integrierte JSON-Bibliothek, die das Verarbeiten von JSON-Daten sehr einfach macht.

# Wie

Um mit JSON in Go zu arbeiten, müssen Sie zuerst die "encoding/json" Bibliothek importieren. Dann können Sie mit den Funktionen "Unmarshal" und "Marshal" JSON-Daten in Go-Strukturen konvertieren und umgekehrt.

Der folgende Codeblock zeigt ein Beispiel für das Unmarshalling von JSON-Daten:

```Go
package main

import (
    "encoding/json"
    "fmt"
)

type Person struct {
    Name string
    Age  int
}

func main() {

    jsonStr := `{"name":"Max Mustermann","age":30}`

    var person Person
    err := json.Unmarshal([]byte(jsonStr), &person)

    if err != nil {
        fmt.Println(err)
    }

    fmt.Println(person.Name)
    fmt.Println(person.Age)
}
```

Die Ausgabe des obigen Codes wird sein:

```
Max Mustermann
30
```

Und hier ist ein Beispiel für das Marshalling von Go-Strukturen in JSON-Daten:

```Go
package main

import (
    "encoding/json"
    "fmt"
)

type Person struct {
    Name string
    Age  int
}

func main() {

    person := Person{"Lisa Schmidt", 25}

    jsonData, err := json.Marshal(person)
    if err != nil {
        fmt.Println(err)
    }

    fmt.Println(string(jsonData))
}
```

Die Ausgabe wird wie folgt sein:

```
{"name":"Lisa Schmidt","age":25}
```

# Deep Dive

Wenn Sie tiefer in das Arbeiten mit JSON in Go eintauchen möchten, gibt es noch einige Tipps und Tricks, die Ihnen helfen können, Ihre Aufgaben effizienter zu erledigen. Zum Beispiel können Sie benutzerdefinierte Tags zu Ihren Go-Strukturen hinzufügen, um sie an die benannten Felder in den JSON-Daten anzupassen. Sie können auch das "encoding/json" Paket verwenden, um direkt mit Streams von JSON-Daten zu arbeiten, statt ganze Daten auf einmal zu konvertieren.

Eine ausführlichere Anleitung zu diesen Techniken und mehr finden Sie in der offiziellen Dokumentation von Go zur JSON-Verarbeitung.

# Siehe auch

- https://golang.org/pkg/encoding/json/
- https://gobyexample.com/json
- https://www.digitalocean.com/community/tutorials/how-to-work-with-json-in-go