---
date: 2024-01-20 15:34:13.802519-07:00
description: "Parsear HTML \xE9 o ato de transformar texto HTML em algo que seu programa\
  \ entenda e possa manipular. Programadores fazem isso para extrair informa\xE7\xF5\
  es,\u2026"
lastmod: '2024-03-13T22:44:46.918396-06:00'
model: unknown
summary: "Parsear HTML \xE9 o ato de transformar texto HTML em algo que seu programa\
  \ entenda e possa manipular. Programadores fazem isso para extrair informa\xE7\xF5\
  es,\u2026"
title: "An\xE1lise de HTML"
weight: 43
---

## What & Why?
Parsear HTML é o ato de transformar texto HTML em algo que seu programa entenda e possa manipular. Programadores fazem isso para extrair informações, automatizar interações com páginas web ou migrar conteúdo para diferentes formatos.

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
