---
date: 2024-02-03 17:50:20.786904-07:00
description: "How to: To parse XML in Go, you use the `encoding/xml` package. This\
  \ package provides the necessary tools to unmarshal (parse) XML into Go structs.\
  \ For\u2026"
lastmod: '2024-03-13T22:44:59.652840-06:00'
model: gpt-4-0125-preview
summary: To parse XML in Go, you use the `encoding/xml` package.
title: Working with XML
weight: 40
---

## How to:


### Parsing XML in Go
To parse XML in Go, you use the `encoding/xml` package. This package provides the necessary tools to unmarshal (parse) XML into Go structs. For example, consider the following XML data representing a book:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

To parse this, define a struct that mirrors the XML structure:

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

    fmt.Printf("Book: %+v\n", book)
}
```

Output:

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Generating XML in Go
To generate an XML document from Go data structures, you again use the `encoding/xml` package. This time you marshal Go structs into XML. Given the previous `Book` struct:

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

## Deep Dive
XML's verbosity and complexity have led to JSON and other formats becoming more popular for many applications. However, XML's ability to represent complex hierarchical data and its widespread use in legacy systems and specific domains (e.g., SOAP services) ensure its relevance.

The `encoding/xml` package in Go provides powerful mechanisms for working with XML, but it's worth noting its limitations. For example, handling XML namespaces can be cumbersome and may require a more detailed understanding of the XML specification than for simpler use cases. Additionally, while Go's static typing and the `encoding/xml` package's marshaling and unmarshaling capabilities are generally efficient, developers might run into challenges with deeply nested structures or when dealing with XML documents that don't map neatly onto Go's type system.

For most modern applications, alternatives like JSON are simpler and more efficient. However, when working in contexts that necessitate XML—due to legacy systems, specific industry standards, or complex data representation needs—Go's standard library provides robust tools to get the job done. As always, the best choice of data format depends on the specific requirements of the application and environment.
