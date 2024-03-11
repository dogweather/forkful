---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:10.662043-07:00
description: "Werken met XML in Go omvat het parsen (lezen) en genereren (schrijven)\
  \ van XML-documenten \u2014 een standaardformaat voor gestructureerde\u2026"
lastmod: '2024-03-11T00:14:24.121042-06:00'
model: gpt-4-0125-preview
summary: "Werken met XML in Go omvat het parsen (lezen) en genereren (schrijven) van\
  \ XML-documenten \u2014 een standaardformaat voor gestructureerde\u2026"
title: Werken met XML
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met XML in Go omvat het parsen (lezen) en genereren (schrijven) van XML-documenten — een standaardformaat voor gestructureerde gegevensuitwisseling. Programmeurs doen dit voor gegevensopslag, configuratie-instellingen of gegevensuitwisseling tussen systemen, vooral in omgevingen waar XML het voorkeurs- of legacygegevensformaat is.

## Hoe:

### XML Parsen in Go
Om XML in Go te parsen, gebruik je het `encoding/xml` pakket. Dit pakket biedt de nodige hulpmiddelen om XML naar Go structs te unmarshalen (parsen). Beschouw bijvoorbeeld de volgende XML-gegevens die een boek voorstellen:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Om dit te parsen, definieer je een struct die de XML-structuur weerspiegelt:

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
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Boek: %+v\n", book)
}
```

Output:

```
Boek: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### XML Genereren in Go
Om een XML-document te genereren vanuit Go-gegevensstructuren, gebruik je opnieuw het `encoding/xml` pakket. Deze keer marshal je Go structs naar XML. Gegeven de voorgaande `Book` struct:

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
        Title:  "Learning Go",
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
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Diepgaande Duik

De uitvoerigheid en complexiteit van XML hebben ertoe geleid dat JSON en andere formaten populairder zijn geworden voor veel toepassingen. Echter, het vermogen van XML om complexe hiërarchische gegevens te vertegenwoordigen en het wijdverbreide gebruik in legacy-systemen en specifieke domeinen (bijv. SOAP-services) garanderen de relevantie ervan.

Het `encoding/xml` pakket in Go biedt krachtige mechanismen om met XML te werken, maar het is de moeite waard om de beperkingen ervan op te merken. Bijvoorbeeld, het omgaan met XML-naamruimten kan lastig zijn en kan een meer gedetailleerd begrip van de XML-specificatie vereisen dan voor eenvoudigere gebruikscases. Daarnaast kunnen ontwikkelaars, hoewel de statische typen van Go en de marshal- en unmarshal-mogelijkheden van het `encoding/xml` pakket over het algemeen efficiënt zijn, uitdagingen tegenkomen met diep geneste structuren of bij het omgaan met XML-documenten die niet netjes op het type systeem van Go passen.

Voor de meeste moderne toepassingen zijn alternatieven zoals JSON eenvoudiger en efficiënter. Echter, wanneer je in contexten werkt die XML noodzakelijk maken - vanwege legacy-systemen, specifieke industriestandaarden of complexe gegevensrepresentatiebehoeften - biedt de standaardbibliotheek van Go robuuste hulpmiddelen om de klus te klaren. Zoals altijd, hangt de beste keuze van gegevensformaat af van de specifieke vereisten van de toepassing en de omgeving.
