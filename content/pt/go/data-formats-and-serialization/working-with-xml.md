---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:04.856656-07:00
description: "Como fazer: Para fazer parsing de XML em Go, voc\xEA utiliza o pacote\
  \ `encoding/xml`. Esse pacote fornece as ferramentas necess\xE1rias para unmarshal\
  \ (fazer o\u2026"
lastmod: '2024-03-13T22:44:46.087209-06:00'
model: gpt-4-0125-preview
summary: "Para fazer parsing de XML em Go, voc\xEA utiliza o pacote `encoding/xml`."
title: Trabalhando com XML
weight: 40
---

## Como fazer:


### Fazendo Parsing de XML em Go
Para fazer parsing de XML em Go, você utiliza o pacote `encoding/xml`. Esse pacote fornece as ferramentas necessárias para unmarshal (fazer o parsing) de XML para structs de Go. Por exemplo, considere os seguintes dados XML representando um livro:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Para fazer o parsing disso, defina uma struct que espelhe a estrutura XML:

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

    fmt.Printf("Livro: %+v\n", book)
}
```

Saída:

```
Livro: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Gerando XML em Go
Para gerar um documento XML a partir de estruturas de dados Go, você usa novamente o pacote `encoding/xml`. Desta vez, você transforma structs de Go em XML. Dada a struct `Book` anterior:

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

Saída:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Aprofundamento
A verbosidade e complexidade do XML levaram ao JSON e outros formatos se tornarem mais populares para muitas aplicações. No entanto, a habilidade do XML em representar dados hierárquicos complexos e seu uso disseminado em sistemas legados e domínios específicos (por exemplo, serviços SOAP) asseguram sua relevância.

O pacote `encoding/xml` em Go fornece mecanismos poderosos para trabalhar com XML, mas vale destacar suas limitações. Por exemplo, lidar com namespaces XML pode ser complicado e pode exigir um entendimento mais detalhado da especificação XML do que para casos de uso mais simples. Além disso, enquanto a tipagem estática de Go e as capacidades de marshaling e unmarshaling do pacote `encoding/xml` são geralmente eficientes, desenvolvedores podem encontrar desafios com estruturas profundamente aninhadas ou ao lidar com documentos XML que não se mapeiam de forma limpa para o sistema de tipos de Go.

Para a maioria das aplicações modernas, alternativas como JSON são mais simples e eficientes. No entanto, quando trabalhando em contextos que necessitam de XML — devido a sistemas legados, padrões industriais específicos ou necessidades complexas de representação de dados — a biblioteca padrão do Go fornece ferramentas robustas para realizar o trabalho. Como sempre, a melhor escolha de formato de dados depende dos requisitos específicos da aplicação e do ambiente.
