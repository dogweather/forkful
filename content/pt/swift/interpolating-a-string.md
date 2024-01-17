---
title:                "Interpolando uma string"
html_title:           "Swift: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Interpolação de string é uma técnica usada por programadores para combinar variáveis e strings de forma a criar uma única string com valores dinâmicos. Isso é especialmente útil para imprimir mensagens personalizadas, gerar URLs ou criar logs.

## Como fazer:

Veja abaixo dois exemplos simples de interpolação de string em Swift:

```
// Exemplo de interpolação de string com uma variável
let nome = "Maria"
let mensagem = "Olá \(nome)! Seja bem-vinda."
print(mensagem)

// Saída: Olá Maria! Seja bem-vinda.

// Exemplo de interpolação de string com expressão
let numA = 5
let numB = 3
let resultado = "\(numA) + \(numB) é igual a \(numA + numB)."
print(resultado)

// Saída: 5 + 3 é igual a 8.
```

## Profundidade

A interpolação de string foi introduzida na linguagem de programação Swift em sua primeira versão, em 2014. Antes disso, os programadores tinham que usar a concatenação de strings para obter o mesmo resultado, o que pode ser mais complicado e menos legível.

Além disso, existem outras formas de combinar variáveis e strings, como a formatação de string, que pode ser mais adequada em certas situações. Porém, a interpolação é a maneira mais simples e direta de criar uma string combinando valores dinâmicos.

Em termos de implementação, a interpolação de string é obtida por meio do uso de delimitadores de marcação no código fonte, que são substituídos pelos valores das variáveis em tempo de execução.

## Veja também:

- [Documentação oficial da interplolação de string em Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID432)
- [Artigo da Apple sobre interpolação de string em Swift](https://developer.apple.com/documentation/swift/string_interpolation)