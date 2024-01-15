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

## Warum

YAML ist eine Text-basierte, strukturierte Datenformat, die häufig für die Konfiguration von Anwendungen oder das Speichern von Daten verwendet wird. Es ist leicht lesbar und ähnelt der menschlichen Sprache, was es zu einer beliebten Wahl für Entwickler macht. Durch die Verwendung von Go können Sie nahtlos mit YAML arbeiten und Ihre Projekte effizienter gestalten.

## Wie geht das?

Um mit YAML in Go zu arbeiten, müssen Sie zunächst das "yaml" Paket importieren. Dann können Sie die "Marshal" und "Unmarshal" Funktionen verwenden, um Daten in und aus YAML-Strukturen zu konvertieren. Zum Beispiel:

```
import (
	"fmt"
	"gopkg.in/yaml.v2"
)

type Person struct {
	Name string `yaml:"name"`
	Age  int    `yaml:"age"`
}

func main() {
	// Daten in YAML konvertieren
	person := &Person{Name: "Max", Age: 25}
	yamlData, err := yaml.Marshal(person)

	if err != nil {
		panic(err)
	}

	// YAML in Struktur konvertieren
	var newPerson Person
	err = yaml.Unmarshal(yamlData, &newPerson)

	if err != nil {
		panic(err)
	}

	fmt.Println(newPerson)
}
```

Die Ausgabe dieses Codes wäre `Name: Max, Age: 25`. Durch die Nutzung von Tags können Sie die Feldnamen in der Struktur an die entsprechenden Schlüssel in der YAML-Datei anpassen.

## Tiefer Einblick

Es gibt viele weitere Funktionen und Optionen, die beim Arbeiten mit YAML in Go verfügbar sind. Zum Beispiel können Sie mit dem "Decoder" und "Encoder" auch Daten von Streams und Dateien lesen und schreiben. Außerdem gibt es zahlreiche Optionen, um das Marshalling und Unmarshalling Ihrer Daten anzupassen. Für eine ausführlichere Erklärung und Beispiele empfehle ich, die offizielle Dokumentation des "yaml" Pakets zu lesen.

## Siehe auch

- Dokumentation des "yaml" Pakets: https://pkg.go.dev/gopkg.in/yaml.v2
- Einführung in Go: https://tour.golang.org/