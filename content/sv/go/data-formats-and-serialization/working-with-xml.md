---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:15.274995-07:00
description: "Hur man g\xF6r: F\xF6r att tolka XML i Go anv\xE4nder du paketet `encoding/xml`.\
  \ Detta paket tillhandah\xE5ller n\xF6dv\xE4ndiga verktyg f\xF6r att avkoda (tolka)\
  \ XML till Go-\u2026"
lastmod: '2024-03-13T22:44:37.418348-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att tolka XML i Go anv\xE4nder du paketet `encoding/xml`."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:


### Tolka XML i Go
För att tolka XML i Go använder du paketet `encoding/xml`. Detta paket tillhandahåller nödvändiga verktyg för att avkoda (tolka) XML till Go-strukturer. Betrakta till exempel följande XML-data som representerar en bok:

```xml
<book id="123">
    <title>Lär dig Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

För att tolka detta definierar du en struktur som speglar XML-strukturen:

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
    <title>Lär dig Go</title>
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

Utskrift:

```
Bok: {XMLName:{Space: Local:book} ID:123 Title:Lär dig Go Author:John Doe Pages:359}
```

### Generera XML i Go
För att generera ett XML-dokument från Go-datastrukturer använder du återigen paketet `encoding/xml`. Den här gången omvandlar du Go-strukturer till XML. Med tanke på den tidigare `Book`-strukturen:

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
        Title:  "Lär dig Go",
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

Utskrift:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Lär dig Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Djupdykning
XML:s ordfyllnad och komplexitet har lett till att JSON och andra format har blivit populärare för många applikationer. Dock säkerställer XML:s förmåga att representera komplex hierarkisk data och dess utbredda användning i äldre system och specifika domäner (t.ex. SOAP-tjänster) dess relevans.

Paketet `encoding/xml` i Go erbjuder kraftfulla mekanismer för att arbeta med XML, men det är värt att notera dess begränsningar. Till exempel kan hantering av XML-namnrymder vara besvärligt och kan kräva en mer detaljerad förståelse för XML-specifikationen än för enklare användningsfall. Dessutom kan utvecklare stöta på utmaningar med djupt nästlade strukturer eller när de hanterar XML-dokument som inte passar snyggt in i Gos typsystem, även om Gos statiska typning och `encoding/xml`-packetets kodnings- och avkodningsförmåga generellt sett är effektiva.

För de flesta moderna applikationer är alternativ som JSON enklare och mer effektiva. Men när man arbetar i sammanhang som kräver XML – på grund av äldre system, specifika branschstandarder eller komplexa datarepresentationsbehov – erbjuder Gos standardbibliotek robusta verktyg för att utföra jobbet. Som alltid beror valet av dataformat på de specifika kraven i applikationen och miljön.
