---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML no Swift: O que, porquê e como?

## O que & Porquê?

*Parsing* HTML é o processo de analisar documentos escritos em HTML para obter informações estruturadas. Programadores fazem isso para extrair dados úteis de páginas da web e manipulá-los conforme necessário.

## Como Fazê-lo:

A biblioteca SwiftSoup é uma opção sólida para analisar HTML no Swift. Aqui está um exemplo simples de como isso funciona:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>Página de teste</title></head><body>Olá Mundo!</body></html>"
    let doc: Document = try SwiftSoup.parse(html)
    let title: Element = try doc.select("title").first()!
    print(try title.text())
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("Erro desconhecido")
}
```

Output:

```Swift
Página de teste
```

## Mergulho Profundo

Desde a invenção do HTML nos anos 90, os programadores têm usado métodos diferentes para analisá-lo. Métodos antigos incluíam combinações de regex e funções de manipulação de strings, mas essas técnicas eram frágeis e propensas a erros. Ferramentas modernas, como o SwiftSoup, tornam o processo muito mais robusto e confiável.

Existem várias alternativas ao SwiftSoup que você pode considerar. Por exemplo, Kanna e Ji são duas bibliotecas populares de parsing HTML para Swift.

O SwiftSoup implicitamente implementa o que é conhecido como um analisador descendente (*top-down parser*), que analisa o HTML começando do topo (o elemento html) e trabalhando para baixo através dos elementos filhos.

## Veja Também

1. [Documentação do SwiftSoup](https://scinfu.github.io/SwiftSoup/)
3. [Parsing HTML com Ji](https://github.com/honghaoz/Ji)