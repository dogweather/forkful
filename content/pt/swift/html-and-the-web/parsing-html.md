---
date: 2024-01-20 15:34:13.802519-07:00
description: "How to: Swift n\xE3o tem suporte nativo para parsear HTML, ent\xE3o\
  \ vamos usar uma biblioteca chamada SwiftSoup. Instale adicionando `SwiftSoup` ao\
  \ seu arquivo\u2026"
lastmod: '2024-03-13T22:44:46.918396-06:00'
model: unknown
summary: "Swift n\xE3o tem suporte nativo para parsear HTML, ent\xE3o vamos usar uma\
  \ biblioteca chamada SwiftSoup."
title: "An\xE1lise de HTML"
weight: 43
---

## How to:
Swift não tem suporte nativo para parsear HTML, então vamos usar uma biblioteca chamada SwiftSoup. Instale adicionando `SwiftSoup` ao seu arquivo `Podfile` ou `Package.swift`.

```Swift
import SwiftSoup

func extrairTitulos(html: String) {
    do {
        let doc = try SwiftSoup.parse(html)
        let titulos = try doc.select("h1").array().map { try $0.text() }
        print(titulos)
    } catch Exception.Error(_, let message) {
        print(message)
    } catch {
        print("Erro desconhecido.")
    }
}

let htmlString = "<html><head><title>Exemplo</title></head><body><h1>Cabeçalho 1</h1><h1>Cabeçalho 2</h1></body></html>"
extrairTitulos(html: htmlString)
```

Output esperado:
```
["Cabeçalho 1", "Cabeçalho 2"]
```

## Deep Dive:
Parsear HTML com Swift é relativamente novo, comparado com linguagens como Python ou Java que têm ferramentas como BeautifulSoup e Jsoup, respectivamente. SwiftSoup é inspirada em Jsoup e oferece uma API similar. É importante saber que fazer parsing de HTML pode ser arriscado se o HTML não for bem formado ou confiável – sempre limpe o conteúdo para evitar ataques como XSS. Alternativas ao SwiftSoup incluem o Kanna e o Alamofire com extensões HTML. Quanto à implementação, a biblioteca SwiftSoup faz um uso pesado de padrões de projetos conhecidos, especialmente o `Visitor Pattern` para percorrer o DOM (Documento Object Model) do HTML.

## See Also:
- [SwiftSoup GitHub](https://github.com/scinfu/SwiftSoup)
- [Tutorial sobre Kanna](https://github.com/tid-kijyun/Kanna)
- [Guia de injeção segura de HTML em Swift](https://developer.apple.com/documentation/foundation/nsattributedstring/1524613-init)
