---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Encontrar o comprimento de uma string significa determinar o número de caracteres que ela contém. Programadores o fazem frequentemente para validação de dados, manipulação de string ou para criar estruturas de controle específicas.

## Como fazer:

Você pode encontrar o comprimento de uma String em Swift usando a propriedade `count`:

```Swift
let minhaString = "Olá, Mundo!"
print(minhaString.count)
```

Esse código imprime '11' porque a string "Olá, Mundo!" contém 11 caracteres.

## Deep Dive:

O Swift não tem uma função integrada para contar caracteres até a sua terceira versão lançada. Antes disso, os programadores tinham que usar funções do NSString. A situação mudou quando o Swift 4 introduziu a propriedade `count` para facilitar a contagem de caracteres.

Você pode usar a propriedade `length` de NSString como alternativa para `count`, mas é uma abordagem menos Swifty e tem algumas desvantagens. Ele conta elementos Unicode e não caracteres visíveis, o que pode produzir resultados inesperados.

Internamente, Swift usa uma representação Unicode para strings. `Count` se comporta diferente dependendo de como a string é codificada. Se uma String for construída usando u+1F1EA e u+1F1F8 (bandeira dos EUA codificada como um par de caracteres regionais), `count` retornará 1.

```Swift
let bandaEUA = "\u{1F1FA}\u{1F1F8}"
print(bandaEUA.count)  // Imprime: 1 
```

## Veja também:

Aqui estão alguns links úteis para entender melhor as strings em Swift:

- Documentação oficial do Swift sobre String e Characters: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Artigo detalhado: "Working with Strings in Swift" por Ray Wenderlich: https://www.raywenderlich.com/7181016-working-with-strings-in-swift
- Artigo sobre a representação de String Unicode em Swift: https://oleb.net/2017/unicode-string/