---
title:                "Lavorare con XML"
aliases:
- /it/go/working-with-xml.md
date:                  2024-02-03T18:13:00.257544-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-xml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con XML in Go comporta l'analisi (lettura) e la generazione (scrittura) di documenti XML, un formato standard per lo scambio di dati strutturati. I programmatori lo fanno per l'archiviazione dei dati, le impostazioni di configurazione, o lo scambio di dati tra sistemi, specialmente in ambienti dove XML è il formato di dati preferito o legacy.

## Come fare:

### Analizzare XML in Go
Per analizzare XML in Go, si utilizza il pacchetto `encoding/xml`. Questo pacchetto fornisce gli strumenti necessari per destrutturare (analizzare) XML in struct di Go. Considera, ad esempio, i seguenti dati XML che rappresentano un libro:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Per analizzare ciò, definisci una struct che riflette la struttura XML:

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

    fmt.Printf("Libro: %+v\n", book)
}
```

Output:

```
Libro: {XMLName:{Space: Local:book} ID:123 Titolo:Learning Go Autore:John Doe Pagine:359}
```

### Generare XML in Go
Per generare un documento XML a partire da strutture dati Go, si utilizza nuovamente il pacchetto `encoding/xml`. Questa volta si trasformano le struct Go in XML. Dato il precedente struct `Book`:

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
        Titolo:  "Learning Go",
        Autore: "John Doe",
        Pagine:  359,
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

## Approfondimento

La verbosità e la complessità di XML hanno portato a JSON e altri formati a diventare più popolari per molte applicazioni. Tuttavia, la capacità di XML di rappresentare dati gerarchici complessi e il suo ampio utilizzo in sistemi legacy e domini specifici (ad es., servizi SOAP) ne assicurano la rilevanza.

Il pacchetto `encoding/xml` in Go offre meccanismi potenti per lavorare con XML, ma vale la pena notare le sue limitazioni. Ad esempio, la gestione degli spazi dei nomi XML può essere laboriosa e potrebbe richiedere una comprensione più dettagliata delle specifiche XML rispetto a casi d'uso più semplici. Inoltre, mentre la tipizzazione statica di Go e le capacità di marshaling e unmarshaling del pacchetto `encoding/xml` sono generalmente efficienti, gli sviluppatori potrebbero incontrare sfide con strutture profondamente annidate o quando si tratta di documenti XML che non si mappano ordinatamente sul sistema di tipi di Go.

Per la maggior parte delle applicazioni moderne, alternative come JSON sono più semplici ed efficienti. Tuttavia, quando si lavora in contesti che richiedono XML – a causa di sistemi legacy, standard specifici del settore o esigenze di rappresentazione dei dati complessi – la libreria standard di Go fornisce strumenti robusti per svolgere il lavoro. Come sempre, la scelta migliore del formato dei dati dipende dai requisiti specifici dell'applicazione e dall'ambiente.
