---
title:                "Arbeiten mit json"
html_title:           "Go: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON steht für "JavaScript Object Notation" und ist ein einfaches Datenformat, das verwendet wird, um Daten zwischen Anwendungen auszutauschen. Programmierer verwenden JSON, um strukturierte Daten zu speichern und zu übertragen, da es einfach zu lesen und zu schreiben ist.

## Wie geht's:
Um mit JSON in Go zu arbeiten, musst du zunächst das Paket "encoding/json" importieren. Dann kannst du die Funktionen "Marshal" und "Unmarshal" nutzen, um Daten in JSON zu konvertieren oder umgekehrt.

Ein Beispiel für das Konvertieren von Daten in JSON:
```Go
type Person struct {
    Name     string `json:"name"`
    Age      int    `json:"age"`
    Location string `json:"location"`
}

person := Person{
    Name:     "Max Mustermann",
    Age:      25,
    Location: "Berlin",
}

jsonData, err := json.Marshal(person)
if err != nil {
    fmt.Println(err)
}

fmt.Println(string(jsonData))
```
Ergebnis:
```bash
{"name":"Max Mustermann","age":25,"location":"Berlin"}
```

Ein Beispiel für das Lesen von Daten aus einer JSON-Datei:
```Go
type Fruit struct {
    Name  string `json:"name"`
    Color string `json:"color"`
}

var fruits []Fruit

jsonFile, err := os.Open("fruits.json")
if err != nil {
    fmt.Println(err)
}

defer jsonFile.Close()

byteData, _ := ioutil.ReadAll(jsonFile)
json.Unmarshal(byteData, &fruits)

fmt.Println(fruits)
```
JSON-Datei "fruits.json":
```bash
[
    {"name": "Apple", "color": "Red"},
    {"name": "Banana", "color": "Yellow"},
    {"name": "Strawberry", "color": "Red"}
]
```
Ergebnis:
```bash
[{Apple Red} {Banana Yellow} {Strawberry Red}]
```

## Tiefentauchen:
JSON wurde ursprünglich von Douglas Crockford in den 1990er Jahren entwickelt und ist seitdem zu einem beliebten Format für den Austausch von Daten geworden. Es gibt auch alternative Datenformate wie XML oder YAML, aber JSON ist aufgrund seiner Einfachheit und Lesbarkeit ein häufig gewähltes Format.

Der "encoding/json" Stdlib in Go bietet auch die Möglichkeit, benutzerdefinierte Marshaler und Unmarshaler zu erstellen, um spezifische Felder zu ignorieren oder zu verarbeiten.

## Siehe auch:
- [Offizielle Dokumentation von "encoding/json" in Go](https://golang.org/pkg/encoding/json/)
- [Einführung in JSON von Mozilla](https://developer.mozilla.org/de/docs/Learn/JavaScript/Objects/JSON)
- [Vergleich von JSON mit anderen Datenformaten](https://www.json.org/xml.html)