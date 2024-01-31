---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:31:31.069282-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML innebærer å parse, skape og manipulere XML-dokumenter ved hjelp av kode. Programmerere gjør dette for datautveksling, konfigurasjonsfiler og webtjenester fordi XMLs lesbarhet og utbredte støtte gjør det til et solid valg for strukturerte data.

## Hvordan:
I Go, bruk pakken `encoding/xml`. La oss parse og generere XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Structs kartlegger til XML-elementer
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Kaffe"}
	coffee.Origin = []string{"Etiopia", "Brasil"}

	// Marshal struct til XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Feil: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshal XML til struct
	data := `
<plant id="27">
  <name>Kaffe</name>
  <origin>Etiopia</origin>
  <origin>Brasil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Feil: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
Eksempel på Utdata:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Kaffe</name>
   <origin>Etiopia</origin>
   <origin>Brasil</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Kaffe Origin:[Etiopia Brasil]}
```

## Dypdykk
XML har vært rundt siden slutten av 90-tallet, designet for storskala elektronisk publisering men ble raskt adoptert for web. Alternativer som JSON har steget frem, rost for sin enkelhet, men XMLs dokumentsvalidering gjennom skjemaer og navneområder forblir kraftfull for komplekse dokumenter. I Go, `encoding/xml` håndterer de fleste oppgavene, men for enorme dokumenter eller strømbehandling, vurder `xml.NewDecoder` og `xml.NewEncoder` for kontroll på lavere nivå og bedre ytelse.

## Se Også
- Gos `encoding/xml` pakke: https://pkg.go.dev/encoding/xml
- XML-opplæring: https://www.w3schools.com/xml/
- Go-blogg om XML: https://blog.golang.org/xml
- Sammenligning mellom JSON og XML: https://www.json.org/xml.html
