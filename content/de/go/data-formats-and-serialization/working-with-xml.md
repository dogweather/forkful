---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:00.012768-07:00
description: "Wie man: Um XML in Go zu parsen, verwenden Sie das Paket `encoding/xml`.\
  \ Dieses Paket stellt die notwendigen Werkzeuge zur Verf\xFCgung, um XML in Go-Structs\u2026"
lastmod: '2024-03-13T22:44:53.314310-06:00'
model: gpt-4-0125-preview
summary: Um XML in Go zu parsen, verwenden Sie das Paket `encoding/xml`.
title: Arbeiten mit XML
weight: 40
---

## Wie man:


### XML in Go parsen
Um XML in Go zu parsen, verwenden Sie das Paket `encoding/xml`. Dieses Paket stellt die notwendigen Werkzeuge zur Verfügung, um XML in Go-Structs zu unmarshalen (parsen). Betrachten Sie zum Beispiel die folgenden XML-Daten, die ein Buch darstellen:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Um dies zu parsen, definieren Sie ein Struct, das die XML-Struktur widerspiegelt:

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

    fmt.Printf("Buch: %+v\n", book)
}
```

Ausgabe:

```
Buch: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### XML in Go generieren
Um ein XML-Dokument aus Go-Datenstrukturen zu generieren, verwenden Sie erneut das Paket `encoding/xml`. Dieses Mal marshalisieren Sie Go-Structs in XML. Gegeben das vorherige `Book`-Struct:

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

Ausgabe:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Tiefere Einblicke
Die Ausführlichkeit und Komplexität von XML haben dazu geführt, dass JSON und andere Formate für viele Anwendungen beliebter geworden sind. Die Fähigkeit von XML, komplexe hierarchische Daten darzustellen, und seine weit verbreitete Verwendung in Legacy-Systemen und spezifischen Bereichen (z.B. SOAP-Dienste) sichern jedoch seine Relevanz.

Das `encoding/xml`-Paket in Go bietet leistungsfähige Mechanismen für die Arbeit mit XML, aber es lohnt sich, seine Einschränkungen zu beachten. Beispielsweise kann die Handhabung von XML-Namespaces umständlich sein und erfordert möglicherweise ein detaillierteres Verständnis der XML-Spezifikation als für einfachere Anwendungsfälle. Darüber hinaus können, obwohl Go's statische Typisierung und die Marshaling- und Unmarshaling-Fähigkeiten des `encoding/xml`-Pakets im Allgemeinen effizient sind, Entwickler bei tief verschachtelten Strukturen oder beim Umgang mit XML-Dokumenten, die sich nicht ordentlich auf Go's Typsystem abbilden lassen, auf Herausforderungen stoßen.

Für die meisten modernen Anwendungen sind Alternativen wie JSON einfacher und effizienter. Wenn jedoch in Kontexten gearbeitet werden muss, die XML erfordern – aufgrund von Legacy-Systemen, spezifischen Branchenstandards oder komplexen Datenrepräsentationsbedürfnissen –, bietet Go's Standardbibliothek robuste Werkzeuge, um die Arbeit zu erledigen. Wie immer hängt die beste Wahl des Datenformats von den spezifischen Anforderungen der Anwendung und der Umgebung ab.
