---
title:                "Parsing html"
html_title:           "Swift: Parsing html"
simple_title:         "Parsing html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

A análise de HTML é um processo importante na programação, pois permite que os desenvolvedores extraiam informações específicas de páginas da web. Isso pode ser útil para criar webscrapers, criar aplicativos que interagem com a web ou analisar dados de páginas da web.

## Como fazer

Para analisar HTML em Swift, podemos usar a biblioteca SwiftSoup. Comece instalando-a em seu projeto através do Cocoapods ou adicionando-a manualmente como um framework. Em seguida, importe a biblioteca em seu arquivo de código:

```Swift
import SwiftSoup
```

Para iniciar a análise, primeiro precisamos recuperar o conteúdo HTML usando uma URL ou um arquivo local:

```Swift
let html = "<html><body><p>Hello, world!</p></body></html>"
do {
    let doc: Document = try SwiftSoup.parse(html)
} catch {
    print("Error parsing HTML.")
}
```

Agora podemos usar o objeto "doc" para selecionar e manipular elementos HTML. Por exemplo, para obter o conteúdo do elemento "p":

```Swift
do {
    let p: Element = try doc.select("p").first()!
    let text: String = try p.text()
    print(text) // Output: Hello, world!
} catch {
    print("Error parsing HTML.")
}
```

## Deep Dive

Para selecionar elementos HTML específicos, podemos usar o seletor CSS. Isso nos permite recuperar elementos com base em suas tags, classes, IDs ou hierarquia. Por exemplo, para obter todos os links em uma página:

```Swift
do {
    let links: [Element] = try doc.select("a").array()
} catch {
    print("Error parsing HTML.")
}
```

Também é possível modificar o conteúdo HTML, adicionando novos elementos ou modificando os existentes. Por exemplo, para adicionar um novo parágrafo ao final do documento:

```Swift
do {
    try doc.select("body").first()!.append("<p>This is a new paragraph.</p>")
    let newHtml: String = try doc.html()
    print(newHtml) // Output: <html><body><p>Hello, world!</p><p>This is a new paragraph.</p></body></html>
} catch {
    print("Error modifying HTML.")
}
```

## Veja também

- Documentação oficial do SwiftSoup: https://github.com/scinfu/SwiftSoup
- Artigo sobre análise de HTML usando Swift: https://medium.com/@jaredjsidwell/parsing-html-in-swift-using-third-party-library-swiftsoup-5d865893df89
- Exemplos práticos de análise de HTML em Swift: https://github.com/Jakelin568/Swift-HTML-Parsing-Example