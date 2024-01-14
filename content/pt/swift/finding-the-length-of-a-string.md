---
title:                "Swift: Encontrando o comprimento de uma string"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já teve que lidar com strings ao programar em Swift, seja para manipulá-las, imprimi-las ou até mesmo para encontrar seu comprimento. O comprimento de uma string é simplesmente o número de caracteres que ela contém e pode ser útil em diversas situações. Neste artigo, vamos explorar por que encontrar o comprimento de uma string pode ser importante e como fazer isso em Swift.

## Como Fazer

Encontrar o comprimento de uma string em Swift é bastante simples. Basta usar a propriedade `count` em sua string, que retornará o número de caracteres que ela possui. Veja um exemplo de código abaixo:

```Swift
let minhaString = "Olá mundo!"
print(minhaString.count)

// Saída: 11
```

Aqui, declaramos uma string e utilizamos a propriedade `count` para retornar seu comprimento. Note que espaços e pontuações também são contados como caracteres.

Outra forma de encontrar o comprimento de uma string é utilizando o método `count(_:)` da classe `NSString`. Ele recebe como parâmetro um `CharacterSet` e retorna o número de caracteres da string que fazem parte desse conjunto. Veja um exemplo:

```Swift
let minhaString = "Aprendendo Swift"
print((minhaString as NSString).count(with: .letters))

// Saída: 15
```

Neste exemplo, declaramos uma string e utilizamos o método `count(_:)` com o parâmetro `.letters`, que contabiliza apenas letras. Dessa forma, conseguimos encontrar o comprimento considerando apenas as letras da nossa string.

## Deep Dive

Agora que sabemos como encontrar o comprimento de uma string, vamos entender um pouco mais sobre como a linguagem Swift lida com strings. Internamente, as strings em Swift são estruturas complexas e podem ser encaradas como coleções de caracteres. Por isso, é possível utilizarmos os mesmos métodos e propriedades que usamos em coleções, como `count` e `isEmpty`.

Além disso, é importante mencionar que a propriedade `count` é uma forma eficiente e otimizada de encontrar o comprimento de uma string. Isso significa que não precisamos nos preocupar com a complexidade do algoritmo utilizado por trás dessa propriedade, já que ela foi projetada para ser rápida e eficiente.

## Veja Também

- [Documentação oficial da linguagem Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial sobre strings em Swift](https://www.hackingwithswift.com/quick-start/understanding-swift/whats-the-difference-between-a-string-and-a-substring-in-swift)
- [Exemplos práticos de manipulação de strings em Swift](https://learnappmaking.com/string-swift-how-to-ios-programming/)

Com este conhecimento, esperamos que você se sinta mais confortável para trabalhar com strings em seus projetos em Swift. Encontrar o comprimento de uma string é apenas uma das muitas operações que podemos fazer com elas, então explore e divirta-se programando!