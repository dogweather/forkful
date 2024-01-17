---
title:                "Convertendo uma string para minúsculas"
html_title:           "Swift: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Converter uma string para letras minúsculas é uma técnica comum usada por programadores para garantir que não haja distinção entre letras maiúsculas e minúsculas em uma string. Isso é especialmente útil ao comparar strings ou ao lidar com entrada do usuário.

## Como fazer:
Para converter uma string para minúsculas em Swift, use o método `lowercased()`. Por exemplo:

```Swift
let exemplo = "Olá, Mundo!"
let exemploMinusculo = exemplo.lowercased()
print(exemploMinusculo) // saída: olá, mundo!
```

## Mergulho Profundo:
Converter strings para minúsculas é uma prática comum no mundo da programação. Originalmente, era necessário fazer isso manualmente, mas com a introdução do Unicode na linguagem de programação Swift, isso se tornou muito mais fácil. Além disso, existem outras opções, como o método `capitalized`, que converte apenas a primeira letra de cada palavra em maiúscula.

## Veja também:
Para mais informações sobre o método `lowercased` e outros métodos de processamento de strings em Swift, consulte a documentação oficial da linguagem: [Strings and Characters - Swift Programming Language](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html).