---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:08.923417-07:00
description: "\xC5 jobbe med XML i Go inneb\xE6rer parsing (lesing) og generering\
  \ (skriving) av XML-dokumenter \u2013 et standardformat for strukturert datautveksling.\u2026"
lastmod: '2024-03-13T22:44:40.294554-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med XML i Go inneb\xE6rer parsing (lesing) og generering (skriving)\
  \ av XML-dokumenter \u2013 et standardformat for strukturert datautveksling."
title: Arbeider med XML
weight: 40
---

## Hva & Hvorfor?

Å jobbe med XML i Go innebærer parsing (lesing) og generering (skriving) av XML-dokumenter – et standardformat for strukturert datautveksling. Programmerere gjør dette for datalagring, konfigurasjonsinnstillinger eller datautveksling mellom systemer, spesielt i miljøer der XML er det foretrukne eller et arvet dataformat.

## Hvordan:

### Parse XML i Go
For å parse XML i Go, bruker du `encoding/xml`-pakken. Denne pakken gir de nødvendige verktøyene for å unmarshal (parse) XML til Go-strukturer. For eksempel, vurder følgende XML-data som representerer en bok:

```xml
<book id="123">
    <title>Læring med Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

For å parse dette, definerer du en struktur som speiler XML-strukturen:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Læring med Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Bok: %+v\n", book)
}
```

Output:

```
Bok: {XMLName:{Space: Local:book} ID:123 Title:Læring med Go Author:John Doe Pages:359}
```

### Generere XML i Go
For å generere et XML-dokument fra Go-datastrukturer, bruker du igjen `encoding/xml`-pakken. Denne gangen marshaller du Go-strukturer til XML. Gitt den tidligere `Book`-strukturen:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Læring med Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

Output:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Læring med Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Dypdykk

XMLs verbosehet og kompleksitet har ført til at JSON og andre formater har blitt mer populære for mange applikasjoner. Imidlertid sikrer XMLs evne til å representere komplekse hierarkiske data og dens utbredte bruk i arvesystemer og spesifikke domener (f.eks., SOAP-tjenester) dets relevans.

`encoding/xml`-pakken i Go gir kraftfulle mekanismer for arbeid med XML, men det er verdt å merke seg dens begrensninger. For eksempel, kan håndtering av XML-navnerom være tungvint og kan kreve en mer detaljert forståelse av XML-spesifikasjonen enn for enklere brukstilfeller. I tillegg, selv om Gos statiske typografi og `encoding/xml`-pakkens marshaling- og unmarshaling-kapasiteter generelt er effektive, kan utviklere støte på utfordringer med dypt nøstede strukturer eller når de håndterer XML-dokumenter som ikke kartlegger pent på Gos typesystem.

For de fleste moderne applikasjoner er alternativer som JSON enklere og mer effektive. Men når du jobber i kontekster som krever XML - på grunn av arvesystemer, spesifikke industrielle standarder, eller komplekse datarepresentasjonsbehov - gir Gos standardbibliotek robuste verktøy for å få jobben gjort. Som alltid avhenger det beste valget av dataformat av de spesifikke kravene til applikasjonen og miljøet.
