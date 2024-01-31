---
title:                "Att arbeta med XML"
date:                  2024-01-26T04:31:58.944101-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka, skapa och manipulera XML-dokument med kod. Programmerare gör detta för datautbyte, konfigurationsfiler och webbtjänster eftersom XML:s läsbarhet och breda stöd gör det till ett stabilt val för strukturerade data.

## Hur man gör:
I Go, använd paketet `encoding/xml`. Låt oss tolka och generera XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Strukturer mappas till XML-element
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Kaffe"}
	coffee.Origin = []string{"Etiopien", "Brasilien"}

	// Omvandla struktur till XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Fel: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Omvandla XML till struktur
	data := `
<plant id="27">
  <name>Kaffe</name>
  <origin>Etiopien</origin>
  <origin>Brasilien</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Fel: %v", err)
		return
	}

	fmt.Printf("\n\nOmvandlad: %+v", p)
}
```
Exempelutskrift:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Kaffe</name>
   <origin>Etiopien</origin>
   <origin>Brasilien</origin>
 </plant>

Omvandlad: {XMLName:{Space: Local:plant} Id:27 Name:Kaffe Origin:[Etiopien Brasilien]}
```

## Fördjupning
XML har funnits sedan slutet av 90-talet, designat för storskalig elektronisk publicering men snabbt antaget för webben. Alternativ som JSON har uppkommit, tillskrivna för enkelhet, men XML:s dokumentvalidering genom scheman och namnrymder är fortfarande kraftfulla för komplexa dokument. I Go, hanterar `encoding/xml` de flesta uppgifter, men för enorma dokument eller strömbehandling, överväg `xml.NewDecoder` och `xml.NewEncoder` för lägre nivåkontroll och bättre prestanda.

## Se även
- Go:s `encoding/xml` paket: https://pkg.go.dev/encoding/xml
- XML-tutorial: https://www.w3schools.com/xml/
- Go-blogg om XML: https://blog.golang.org/xml
- Jämförelse mellan JSON och XML: https://www.json.org/xml.html
