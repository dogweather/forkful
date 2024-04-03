---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:56.116242-07:00
description: "Trabajar con XML en Go implica analizar (leer) y generar (escribir)\
  \ documentos XML, un formato est\xE1ndar para el intercambio de datos estructurados.\
  \ Los\u2026"
lastmod: '2024-03-13T22:44:58.496858-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con XML en Go implica analizar (leer) y generar (escribir) documentos\
  \ XML, un formato est\xE1ndar para el intercambio de datos estructurados."
title: Trabajando con XML
weight: 40
---

## Cómo hacerlo:


### Analizando XML en Go
Para analizar XML en Go, se utiliza el paquete `encoding/xml`. Este paquete proporciona las herramientas necesarias para deserializar (analizar) XML en estructuras de Go. Por ejemplo, considera los siguientes datos XML que representan un libro:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Para analizar esto, define una estructura que refleje la estructura XML:

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

Salida:

```
Libro: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Generando XML en Go
Para generar un documento XML a partir de estructuras de datos de Go, nuevamente usas el paquete `encoding/xml`. Esta vez marshalizas (serializas) las estructuras de Go en XML. Dada la anterior estructura `Book`:

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

Salida:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Profundización
La verbosidad y complejidad de XML ha llevado a que JSON y otros formatos se vuelvan más populares para muchas aplicaciones. Sin embargo, la capacidad de XML para representar datos jerárquicos complejos y su uso generalizado en sistemas heredados y dominios específicos (por ejemplo, servicios SOAP) aseguran su relevancia.

El paquete `encoding/xml` en Go proporciona mecanismos poderosos para trabajar con XML, pero vale la pena señalar sus limitaciones. Por ejemplo, manejar espacios de nombres XML puede ser engorroso y puede requerir un entendimiento más detallado de la especificación XML que para casos de uso más simples. Además, mientras que la tipificación estática de Go y las capacidades de marshalización y desmarshalización del paquete `encoding/xml` son generalmente eficientes, los desarrolladores podrían enfrentar desafíos con estructuras profundamente anidadas o al lidiar con documentos XML que no se mapean limpiamente al sistema de tipos de Go.

Para la mayoría de las aplicaciones modernas, alternativas como JSON son más simples y eficientes. Sin embargo, al trabajar en contextos que necesitan XML, debido a sistemas heredados, estándares industriales específicos o necesidades de representación de datos complejos, la biblioteca estándar de Go proporciona herramientas robustas para hacer el trabajo. Como siempre, la mejor elección de formato de datos depende de los requisitos específicos de la aplicación y el entorno.
