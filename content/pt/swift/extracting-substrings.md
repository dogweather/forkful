---
title:                "Extraindo subcadeias"
html_title:           "Swift: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Extração de substrings é o processo de selecionar e retornar uma parte específica de uma string. Os programadores fazem isso para manipular e trabalhar com diferentes partes de uma string separadamente.

## Como fazer:

```Swift
// Criar uma string
let frase = "Eu amo programação em Swift"

// Usando a função substring para extrair uma parte da string
let programa = frase.substring(with: Range<String.Index>(uncheckedBounds: (lower: String.Index(encodedOffset: 12), upper: String.Index(encodedOffset: 21))))

// Output: "programação"
```

## Mergulho Profundo:

(1) A extração de substrings tem sido uma técnica amplamente utilizada desde os primeiros dias da programação. (2) Uma alternativa à função substring é o uso de operadores de índice, como o ```[ ]``, para acessar partes de uma string. (3) Internamente, a função substring faz uso de uma estrutura de dados chamada "index collection" para armazenar as posições dos caracteres da string e retornar o trecho desejado.

## Veja também:

- Documentação oficial da Swift: [Substring](https://developer.apple.com/documentation/swift/substring)
- Tutoriais de programação em Swift: [Ray Wenderlich](https://www.raywenderlich.com/swift)