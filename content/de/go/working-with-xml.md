---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:31:12.120824-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML beinhaltet das Parsen, Erstellen und Manipulieren von XML-Dokumenten mithilfe von Code. Programmierer tun dies für den Datenaustausch, Konfigurationsdateien und Webdienste, da die Lesbarkeit und weit verbreitete Unterstützung von XML es zu einer soliden Wahl für strukturierte Daten machen.

## Wie:
In Go verwendet man das `encoding/xml` Paket. Lassen Sie uns XML parsen und generieren.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Structs entsprechen XML-Elementen
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Kaffee"}
	coffee.Origin = []string{"Äthiopien", "Brasilien"}

	// Struktur in XML umwandeln
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Fehler: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// XML in Struktur umwandeln
	data := `
<plant id="27">
  <name>Kaffee</name>
  <origin>Äthiopien</origin>
  <origin>Brasilien</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Fehler: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
Beispielausgabe:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Kaffee</name>
   <origin>Äthiopien</origin>
   <origin>Brasilien</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Kaffee Origin:[Äthiopien Brasilien]}
```

## Vertiefung
XML gibt es seit den späten 90ern, entworfen für großangelegtes elektronisches Publizieren, aber schnell für das Web adaptiert. Alternativen wie JSON sind aufgetaucht, die für ihre Einfachheit gelobt werden, aber XMLs Dokumentenvalidierung durch Schemas und Namensräume bleiben mächtig für komplexe Dokumente. In Go handhabt `encoding/xml` die meisten Aufgaben, aber für riesige Dokumente oder Stream-Verarbeitung sollte man `xml.NewDecoder` und `xml.NewEncoder` für tiefere Kontrolle und bessere Leistung in Betracht ziehen.

## Siehe auch
- Go's `encoding/xml` Paket: https://pkg.go.dev/encoding/xml
- XML-Tutorial: https://www.w3schools.com/xml/
- Go Blog über XML: https://blog.golang.org/xml
- Vergleich zwischen JSON und XML: https://www.json.org/xml.html
