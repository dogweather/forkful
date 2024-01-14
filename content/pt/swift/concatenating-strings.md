---
title:                "Swift: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma técnica muito útil na programação Swift que permite unir duas ou mais strings em uma única string. Isso é especialmente útil quando você precisa criar mensagens personalizadas ou manipular dados de entrada do usuário. Aprender a concatenar strings pode tornar o seu código mais eficiente e fácil de ler.

## Como fazer

A concatenação de strings é bastante simples de realizar em Swift. Você pode usar o operador "+" para unir duas strings ou usar o método "append" para adicionar uma string ao final de outra. Veja um exemplo de como usar o operador "+" para concatenar strings:

``` Swift 
let primeiraString = "Olá"
let segundaString = "mundo!"
let mensagem = primeiraString + segundaString
print(mensagem)
```

Este código irá resultar na seguinte saída: "Olá mundo!".

Também é possível concatenar mais de duas strings de uma vez, basta adicionar o operador "+" entre cada uma delas. Por exemplo:

``` Swift
let primeiraString = "Eu"
let segundaString = "amo"
let terceiraString = "programar"
let mensagem = primeiraString + " " + segundaString + " " + terceiraString
print(mensagem)
```

Este código irá resultar na seguinte saída: "Eu amo programar".

Existem ainda outras formas de concatenar strings em Swift, como a utilização de templates de string ou operadores compostos. Para saber mais, consulte os links da seção "Veja também".

## Aprofundando-se

A concatenação de strings pode parecer simples, mas é importante entender como ela funciona nos bastidores. Em Swift, strings são representadas por uma estrutura de dados que armazena cada caractere individualmente e, quando concatenadas, essa estrutura precisa ser recriada para acomodar a nova string. Por isso, é sempre importante pesquisar sobre desempenho ao usar muitas concatenações em um mesmo trecho de código.

Outro aspecto importante é a utilização de diferentes tipos de dados em uma concatenação. Ao unir uma string com um número, por exemplo, é preciso converter o número para uma string utilizando o método "String()". Caso contrário, o compilador pode gerar um erro.

## Veja também

- [Documentação da Apple sobre strings em Swift](https://developer.apple.com/documentation/swift/string)
- [Artigo sobre concatenação de strings em Swift](https://medium.com/@JohnSundell/using-the-string-plus-operator-in-swift-2479a2544e69)
- [Tutorial sobre templates de string em Swift](https://learnappmaking.com/string-interpolation-swift-how-to/)