---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:06.925317-07:00
description: "Travailler avec XML en Go implique d'analyser (lire) et de g\xE9n\xE9\
  rer (\xE9crire) des documents XML \u2013 un format standard pour l'\xE9change de\
  \ donn\xE9es structur\xE9es.\u2026"
lastmod: '2024-02-25T18:49:54.061256-07:00'
model: gpt-4-0125-preview
summary: "Travailler avec XML en Go implique d'analyser (lire) et de g\xE9n\xE9rer\
  \ (\xE9crire) des documents XML \u2013 un format standard pour l'\xE9change de donn\xE9\
  es structur\xE9es.\u2026"
title: Travailler avec XML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec XML en Go implique d'analyser (lire) et de générer (écrire) des documents XML – un format standard pour l'échange de données structurées. Les programmeurs le font pour le stockage de données, les paramètres de configuration, ou l'échange de données entre systèmes, en particulier dans les environnements où XML est le format de données préféré ou hérité.

## Comment faire :

### Analyser XML en Go
Pour analyser XML en Go, vous utilisez le paquet `encoding/xml`. Ce paquet fournit les outils nécessaires pour démarshaliser (analyser) le XML en structures Go. Par exemple, considérez les données XML suivantes représentant un livre :

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

Pour analyser cela, définissez une structure qui reflète la structure XML :

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

    fmt.Printf("Livre : %+v\n", book)
}
```

Sortie :

```
Livre : {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Générer XML en Go
Pour générer un document XML à partir des structures de données Go, vous utilisez à nouveau le paquet `encoding/xml`. Cette fois, vous marshalisez les structures Go en XML. Étant donné la structure `Book` précédente :

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

Sortie :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## Plongée Profonde

La verbosité et la complexité de XML ont conduit à ce que JSON et d'autres formats deviennent plus populaires pour de nombreuses applications. Cependant, la capacité de XML à représenter des données hiérarchiques complexes et son utilisation répandue dans les systèmes hérités et certains domaines spécifiques (par exemple, les services SOAP) assurent sa pertinence.

Le paquet `encoding/xml` en Go offre des mécanismes puissants pour travailler avec XML, mais il convient de noter ses limitations. Par exemple, gérer les espaces de noms XML peut être fastidieux et peut nécessiter une compréhension plus détaillée de la spécification XML que pour des cas d'utilisation plus simples. De plus, bien que le typage statique de Go et les capacités de marshaling et de démarshaling du paquet `encoding/xml` soient généralement efficaces, les développeurs pourraient se heurter à des défis avec des structures profondément imbriquées ou lorsqu'ils traitent avec des documents XML qui ne se mappent pas proprement sur le système de typage de Go.

Pour la plupart des applications modernes, des alternatives comme JSON sont plus simples et plus efficaces. Cependant, lorsqu'on travaille dans des contextes qui nécessitent XML — en raison de systèmes hérités, de normes industrielles spécifiques, ou de besoins complexes de représentation de données — la bibliothèque standard de Go fournit des outils robustes pour faire le travail. Comme toujours, le meilleur choix de format de données dépend des exigences spécifiques de l'application et de l'environnement.
