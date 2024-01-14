---
title:                "Swift: Extraindo subtrings"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Porque

A extração de substrings é uma ferramenta útil na linguagem de programação Swift que permite aos desenvolvedores obter partes específicas de uma string. Isso pode ser útil para manipular dados ou criar interfaces de usuário dinâmicas.

## Como Fazer

Aqui estão alguns exemplos de como extrair substrings em Swift:

```Swift
// Criando uma string para ser manipulada
let frase = "A programação é emocionante!"

// Obtendo a primeira letra da string
let primeiraLetra = frase.prefix(1)

// Obtendo a última letra da string
let ultimaLetra = frase.suffix(1)

// Obtendo as três primeiras letras da string
let tresPrimeirasLetras = frase[..<frase.index(frase.startIndex, offsetBy: 3)]

// Obtendo as três últimas letras da string
let tresUltimasLetras = frase.suffix(3)
```

Ao executar esse código, a saída será:

```
primeiraLetra = "A"
ultimaLetra = "!"
tresPrimeirasLetras = "A p"
tresUltimasLetras = "te!"
```

Esses são apenas alguns exemplos básicos de como extrair substrings em Swift. Mas existem muitas outras funções e métodos disponíveis que permitem uma maior flexibilidade na manipulação de strings.

## Deep Dive

A extração de substrings é possível graças à estrutura de dados String, que armazena uma sequência de caracteres em Swift. Essa estrutura é indexada, o que significa que cada caractere da string tem uma posição específica. Isso permite que os desenvolvedores acessem caracteres individuais ou um intervalo de caracteres a partir dessa posição.

Além disso, a linguagem Swift possui muitas funções e métodos úteis para manipulação de strings, incluindo as mencionadas nos exemplos acima, bem como outras como `prefix(upTo:)` e `dropFirst(_:)`. Com essas ferramentas, os desenvolvedores podem extrair substrings com base em critérios específicos, facilitando a manipulação de dados e a criação de interfaces de usuário dinâmicas.

## Veja Também

- [Documentação oficial do Swift sobre strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial da Ray Wenderlich sobre manipulação de strings em Swift](https://www.raywenderlich.com/263380/strings-in-swift-a-tutorial)
- [Artigo do Medium sobre extração de substrings em Swift](https://medium.com/@abhimuralidharan/how-to-use-substring-with-a-string-in-swift-4e6791d02f93)