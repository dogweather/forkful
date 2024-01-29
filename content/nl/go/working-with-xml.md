---
title:                "Werken met XML"
date:                  2024-01-28T22:11:19.125509-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met XML omvat het parsen, creëren en manipuleren van XML-documenten met behulp van code. Programmeurs doen dit voor gegevensuitwisseling, configuratiebestanden en webdiensten omdat de leesbaarheid van XML en de brede ondersteuning ervan het een solide keuze maken voor gestructureerde gegevens.

## Hoe:
Gebruik in Go het `encoding/xml` pakket. Laten we XML parsen en genereren.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Structs worden gekoppeld aan XML-elementen
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Koffie"}
	coffee.Origin = []string{"Ethiopië", "Brazilië"}

	// Marshal struct naar XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Fout: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshal XML naar struct
	data := `
<plant id="27">
  <name>Koffie</name>
  <origin>Ethiopië</origin>
  <origin>Brazilië</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Fout: %v", err)
		return
	}

	fmt.Printf("\n\nGedemarshalled: %+v", p)
}
```
Voorbeelduitvoer:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Koffie</name>
   <origin>Ethiopië</origin>
   <origin>Brazilië</origin>
 </plant>

Gedemarshalled: {XMLName:{Space: Local:plant} Id:27 Name:Koffie Origin:[Ethiopië Brazilië]}
```

## Diepere duik
XML bestaat al sinds de late jaren '90, ontworpen voor grootschalige elektronische publicaties, maar werd snel aangenomen voor het web. Alternatieven zoals JSON zijn opgekomen, geprezen om hun eenvoud, maar de documentvalidatie van XML door schema's en namespaces blijft krachtig voor complexe documenten. In Go behandelt `encoding/xml` de meeste taken, maar voor enorme documenten of stroomverwerking, overweeg `xml.NewDecoder` en `xml.NewEncoder` voor meer controle op laag niveau en betere prestaties.

## Zie ook
- Go's `encoding/xml` pakket: https://pkg.go.dev/encoding/xml
- XML tutoriaal: https://www.w3schools.com/xml/
- Go blog over XML: https://blog.golang.org/xml
- Vergelijking tussen JSON en XML: https://www.json.org/xml.html
