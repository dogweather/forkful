---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:15.375664-07:00
description: "Jak to zrobi\u0107: Aby parsowa\u0107 XML w Go, u\u017Cywa si\u0119\
  \ pakietu `encoding/xml`. Pakiet ten dostarcza niezb\u0119dne narz\u0119dzia do\
  \ deserializacji (parsowania) XML do\u2026"
lastmod: '2024-03-13T22:44:34.880686-06:00'
model: gpt-4-0125-preview
summary: "Aby parsowa\u0107 XML w Go, u\u017Cywa si\u0119 pakietu `encoding/xml`."
title: Praca z XML
weight: 40
---

## Jak to zrobić:


### Parsowanie XML w Go
Aby parsować XML w Go, używa się pakietu `encoding/xml`. Pakiet ten dostarcza niezbędne narzędzia do deserializacji (parsowania) XML do struktur Go. Na przykład, rozważ poniższe dane XML reprezentujące książkę:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Aby to sparsować, zdefiniuj strukturę, która odzwierciedla strukturę XML:

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

    fmt.Printf("Książka: %+v\n", book)
}
```

Wyjście:

```
Książka: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Generowanie XML w Go
Aby wygenerować dokument XML ze struktur danych Go, ponownie używa się pakietu `encoding/xml`. Tym razem serializuje się struktury Go do XML. Mając poprzednią strukturę `Book`:

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

Wyjście:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Dogłębna analiza
Rozwlekłość i złożoność XML spowodowały, że JSON i inne formaty stały się bardziej popularne dla wielu zastosowań. Jednak zdolność XML do reprezentowania złożonych hierarchicznych danych i jego powszechne używanie w systemach dziedzicznych i określonych domenach (np. usługi SOAP) zapewniają jego istotność.

Pakiet `encoding/xml` w Go dostarcza potężne mechanizmy do pracy z XML, ale warto zauważyć jego ograniczenia. Na przykład, obsługa przestrzeni nazw XML może być uciążliwa i może wymagać dokładniejszego zrozumienia specyfikacji XML niż w prostszych przypadkach użycia. Dodatkowo, chociaż statyczne typowanie Go i zdolności serializacji oraz deserializacji pakietu `encoding/xml` są generalnie wydajne, programiści mogą napotkać wyzwania przy bardzo zagnieżdżonych strukturach lub przy pracy z dokumentami XML, które nie mapują się czysto na system typów Go.

Dla większości nowoczesnych aplikacji alternatywy takie jak JSON są prostsze i bardziej wydajne. Jednak, pracując w kontekstach, które wymagają XML - ze względu na systemy dziedziczne, konkretne standardy branżowe lub złożone potrzeby reprezentacji danych - biblioteka standardowa Go dostarcza solidnych narzędzi do wykonania zadania. Jak zawsze, najlepszy wybór formatu danych zależy od konkretnych wymagań aplikacji i środowiska.
