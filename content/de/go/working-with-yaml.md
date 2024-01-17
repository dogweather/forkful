---
title:                "Arbeiten mit YAML"
html_title:           "Go: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML (Yet Another Markup Language) ist eine lesbar-formatierte Datenstruktur, die von Programmierern häufig verwendet wird, um Daten in einer einfachen und menschenlesbaren Weise zu speichern. Sie wird oft für Konfigurationsdateien verwendet und bietet eine praktische Alternative zu komplexen und schwer zu lesenden Datei- und Datenformaten.

## Wie geht's?
Go bietet eine eingebaute Unterstützung für das Arbeiten mit YAML. Hier ist ein Beispiel, wie Sie Daten in einer YAML-Datei speichern und lesen können:
```
package main

import (
    "fmt"
    "gopkg.in/yaml.v2"
    "os"
)

type person struct {
    Name  string `yaml:"name"`
    Age   int    `yaml:"age"`
    Email string `yaml:"email"`
}

func main() {

    // Daten strukturieren
    data := map[string]person{
        "1": {
            Name:  "Max Mustermann",
            Age:   30,
            Email: "max@mustermann.com",
        },
        "2": {
            Name:  "Anna Schmidt",
            Age:   25,
            Email: "anna@schmidt.com",
        },
    }

    // YAML-Datei erstellen
    file, err := os.Create("./test.yaml")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Daten in YAML-Format codieren und in die Datei schreiben
    err = yaml.NewEncoder(file).Encode(data)
    if err != nil {
        panic(err)
    }

    // YAML-Datei lesen
    file, err = os.Open("./test.yaml")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Daten aus Datei in eine Variable decodieren
    var newData map[string]person
    err = yaml.NewDecoder(file).Decode(&newData)
    if err != nil {
        panic(err)
    }

    // Daten ausgeben
    fmt.Println(newData["1"].Name)
    fmt.Println(newData["1"].Age)
    fmt.Println(newData["1"].Email)
    fmt.Println(newData["2"].Name)
    fmt.Println(newData["2"].Age)
    fmt.Println(newData["2"].Email)

}
```

Das obige Beispiel erstellt eine YAML-Datei mit den Informationen von zwei Personen und liest dann diese Datei und gibt die gespeicherten Daten in der Konsole aus.

## Tiefer Tauchgang
YAML wurde ursprünglich von Clark Evans im Jahr 2001 entwickelt und ist eine Erweiterung der bekannten Markup-Sprache "YAML Ain't Markup Language". Es bietet eine leichte Syntax und ist einfach zu lesen und zu interpretieren. Alternativen zu YAML sind z.B. JSON oder XML, jedoch bietet YAML eine bessere Lesbarkeit und ist deshalb bei Entwicklern sehr beliebt. In Go wird YAML durch die Bibliothek "gopkg.in/yaml.v2" unterstützt.

## Siehe auch
- [YAML offizielle Website](https://yaml.org/)
- [YAML vs JSON vs XML](https://www.geeksforgeeks.org/difference-json-vs-xml-vs-yaml/)
- [Dokumentation der YAML-Bibliothek in Go](https://pkg.go.dev/gopkg.in/yaml.v2)