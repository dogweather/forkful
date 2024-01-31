---
title:                "Lavorare con XML"
date:                  2024-01-26T04:31:34.548558-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con XML coinvolge il parsing, la creazione e la manipolazione di documenti XML usando codice. I programmatori lo fanno per lo scambio di dati, file di configurazione e servizi web perché la leggibilità di XML e il suo ampio supporto lo rendono una scelta solida per i dati strutturati.

## Come fare:
In Go, usare il pacchetto `encoding/xml`. Analizziamo e generiamo XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Le struct mappano agli elementi XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Caffè"}
	coffee.Origin = []string{"Etiopia", "Brasile"}

	// Marshalling della struct in XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Errore: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshalling dell'XML in struct
	data := `
<plant id="27">
  <name>Caffè</name>
  <origin>Etiopia</origin>
  <origin>Brasile</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Errore: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
Esempio di output:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Caffè</name>
   <origin>Etiopia</origin>
   <origin>Brasile</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Caffè Origin:[Etiopia Brasile]}
```

## Approfondimento
XML esiste dalla fine degli anni '90, progettato per l'editoria elettronica su larga scala ma rapidamente adottato per il web. Alternative come JSON sono emerse, lodate per la semplicità, ma la validazione dei documenti attraverso schemi e namespace di XML rimane potente per documenti complessi. In Go, `encoding/xml` gestisce la maggior parte dei compiti, ma per documenti enormi o per l'elaborazione in streaming, considerare `xml.NewDecoder` e `xml.NewEncoder` per un controllo di basso livello e prestazioni migliori.

## Vedi anche
- Il pacchetto `encoding/xml` di Go: https://pkg.go.dev/encoding/xml
- Tutorial su XML: https://www.w3schools.com/xml/
- Blog di Go su XML: https://blog.golang.org/xml
- Confronto tra JSON e XML: https://www.json.org/xml.html
