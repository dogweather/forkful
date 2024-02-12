---
title:                "Extraindo substrings"
aliases:
- /pt/swift/extracting-substrings/
date:                  2024-01-20T17:46:32.890975-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Extrair substrings é o ato de pegar pedaços específicos de uma string maior. Programadores fazem isso para manipular, analisar, e processar dados de texto baseados em partes relevantes de informação.

## How to:

Swift tem um jeito direto de lidar com substrings. Aqui estão alguns exemplos:

```Swift
// Primeiro, defina uma string original
let frase = "Olá, programadores Swift!"

// Use o método 'prefix' para pegar os primeiros 3 caracteres
let inicio = frase.prefix(3)
print(inicio)  // Output: Olá

// Utilize o índice para cortar uma parte do meio
let indiceInicial = frase.index(frase.startIndex, offsetBy: 5)
let indiceFinal = frase.index(frase.startIndex, offsetBy: 19)
let meio = frase[indiceInicial..<indiceFinal]
print(meio)  // Output: programadores

// Utilize 'suffix' para os últimos 6 caracteres
let fim = frase.suffix(6)
print(fim)  // Output: Swift!
```

## Deep Dive:

Historicamente, manipular strings em programação sempre foi uma tarefa comum e crítica. Em Swift, desde suas primeiras versões, tratou-se de fornecer um conjunto de ferramentas poderoso para string handling. Antes de Swift 4, trabalhar com substrings era um pouco mais complicado e menos intuitivo.

Em alternativa ao uso direto de `prefix`, `suffix`, e índices, métodos como `range(of:)` e regex podem ser usados para extrações mais complexas. Porém, cuidado com a eficiência. Substrings em Swift são otimizados para compartilhar a memória com a string original quando possível, o que significa que não há custos na criação de uma nova string até que se faça uma mutação.

A implementação da extração de substrings no Swift é cuidadosamente projetada para ser tanto eficiente quanto segura. Por exemplo, ao invés de usar índices de inteiros que podem causar erros de 'index out of range', Swift usa seu próprio tipo `String.Index` para garantir acesso seguro aos caracteres.

## See Also:

- Os documentos oficiais da Apple sobre String e Character em Swift: [Swift Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Para buscar conhecimento avançado, sugiro: [Advanced String Processing in Swift](https://www.objc.io/books/advanced-swift/)
