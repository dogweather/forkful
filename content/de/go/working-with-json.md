---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON steht für JavaScript Object Notation und ist ein Format zum Austausch von Datenobjekten zwischen Server und Client. Entwickler verwenden es, weil es leichtgewichtig, leicht zu lesen und sprachunabhängig ist.

## How to:
Go bietet eingebaute Unterstützung für JSON mit dem `encoding/json`-Paket. Hier ein Beispiel, wie man JSON in Go (de)serialisiert:

```Go
package main

import (
	"encoding/json"
	"fmt"
)

// Definiert eine Struktur, die der JSON entspricht
type Person struct {
	Name string `json:"name"`
	Alter int    `json:"alter"`
}

func main() {
	// JSON serialisieren
	p := Person{Name: "Max", Alter: 25}
	jsonData, err := json.Marshal(p)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(string(jsonData))

	// JSON deserialisieren
	var person Person
	err = json.Unmarshal(jsonData, &person)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Printf("%+v\n", person)
}
```

### Ausgabe
```
{"name":"Max","alter":25}
{Name:Max Alter:25}
```

## Deep Dive
JSON wurde Anfang der 2000er Jahre populär und ist heute eines der meistgenutzten Formate für Web APIs. Es gibt Alternativen wie XML, aber JSON ist kompakter und schneller zu parsen. Go's `encoding/json`-Paket verwendet Reflektion, um JSON in Go-Strukturen umzuwandeln und umgekehrt, was bei großen Datenmengen zu Leistungseinbußen führen kann. Für Hochleistungssysteme könnten Entwickler andere Bibliotheken wie `jsoniter` in Erwägung ziehen.

## See Also
- Die offizielle Go Dokumentation zum `encoding/json`-Paket: https://pkg.go.dev/encoding/json
- JSON und Go: https://blog.golang.org/json
- Alternative JSON-Pakete für Go: https://github.com/json-iterator/go
