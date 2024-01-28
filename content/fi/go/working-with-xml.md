---
title:                "XML:n käsittely"
date:                  2024-01-26T04:31:27.920820-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
XML:n kanssa työskentely sisältää XML-dokumenttien jäsentämisen, luomisen ja manipuloinnin koodin avulla. Ohjelmistokehittäjät tekevät näin datan vaihdon, asetustiedostojen ja verkkopalveluiden vuoksi, koska XML:n luettavuus ja laaja tuki tekevät siitä vankan valinnan rakenteelliselle datalle.

## Kuinka:
Go:ssa, käytä `encoding/xml` -pakettia. Aloitetaan jäsentämällä ja luomalla XML:ää.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Rakenteet vastaavat XML-elementtejä
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Marshaloi rakenne XML:ksi
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Virhe: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Purkaa XML rakenteeksi
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Virhe: %v", err)
		return
	}

	fmt.Printf("\n\nPurettu: %+v", p)
}
```
Esimerkkituloste:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Purettu: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## Syväsukellus
XML on ollut olemassa 90-luvun lopulta lähtien, suunniteltu suurimuotoista sähköistä julkaisua varten, mutta se otettiin nopeasti käyttöön webissä. Vaihtoehtoja kuten JSON on noussut esiin yksinkertaisuutensa vuoksi, mutta XML:n dokumentin validointi skeemojen ja nimiavaruuden kautta pysyy voimakkaana monimutkaisten dokumenttien kohdalla. Go:ssa `encoding/xml` hoitaa useimmat tehtävät, mutta valtavien dokumenttien tai virtaprosessoinnin osalta, harkitse `xml.NewDecoder`in ja `xml.NewEncoder`in käyttöä alempitasoiseen hallintaan ja parempaan suorituskykyyn.

## Katso myös
- Gon `encoding/xml` -paketti: https://pkg.go.dev/encoding/xml
- XML-opas: https://www.w3schools.com/xml/
- Go blogin XML:stä: https://blog.golang.org/xml
- Vertailu JSONin ja XML:n välillä: https://www.json.org/xml.html
