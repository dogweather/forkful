---
title:                "Buscando e substituindo texto"
html_title:           "Swift: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Procurar e substituir texto é uma tarefa comum para programadores. Trata-se de encontrar determinado conteúdo em um texto e alterá-lo por outro. Isso é importante pois permite a correção de erros ou a atualização de informações em um grande volume de texto de forma rápida e eficiente.

## Como fazer:
Para buscar e substituir texto em Swift, podemos utilizar o método nativo `replacingOccurrences(of:with:)` da classe `String`. Veja o exemplo abaixo:

```Swift
let texto = "Olá, mundo!"
let novoTexto = texto.replacingOccurrences(of: "mundo", with: "pessoal")
print(novoTexto) // Saída: Olá, pessoal!
```

## Mergulhando fundo:
Historicamente, a busca e substituição de texto foi uma tarefa árdua, exigindo o uso de expressões regulares ou percorrer o texto caractere por caractere. Com o avanço da tecnologia, surgiram ferramentas e tecnologias que facilitam essa tarefa, como o `replacingOccurrences(of:with:)` em Swift. Outra alternativa popular é o uso da ferramenta `sed` no Terminal.

## Veja também:
- [Documentação do método `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/foundation/nsstring/1412435-replacingoccurrences)
- [Tutorial sobre expressões regulares em Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
- [Como usar o `sed` no Terminal](https://www.musculogeeks.com/commandes-shell-sed/)