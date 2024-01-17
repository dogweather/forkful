---
title:                "Comparando duas datas"
html_title:           "Swift: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Comparar duas datas é uma tarefa muito comum entre os programadores. Isso permite determinar qual data ocorreu antes ou depois da outra, o que pode ser útil em diversas situações de programação. Por exemplo, pode ser usado para verificar a idade de uma pessoa, calcular o tempo de vida de um objeto ou determinar a data de validade de um produto.

## Como fazer:
Existem várias maneiras de comparar duas datas em Swift. Uma delas é usando o operador de igualdade `==`, que verifica se duas datas são iguais. Por exemplo:

```Swift
let data1 = Date()
let data2 = Date()

if data1 == data2 {
    print("As datas são iguais")
} else {
    print("As datas são diferentes")
}
```

Outra maneira é usar o método `compare`, que retorna um `ComparisonResult` indicando se as datas são iguais, anteriores ou posteriores. Por exemplo:

```Swift
let data1 = Date()
let data2 = Date(timeIntervalSinceNow: 60) // adiciona 60 segundos à data atual

let resultado = data1.compare(data2)

if resultado == .orderedSame {
    print("As datas são iguais")
} else if resultado == .orderedAscending {
    print("A data1 ocorreu antes da data2")
} else {
    print("A data2 ocorreu depois da data1")
}
```

## Mergulhe fundo:
Comparar datas é uma prática comum em programação desde os primórdios da computação. Muitas linguagens de programação, como C e Java, possuem esse recurso em suas bibliotecas padrão. Além disso, existem outras formas de comparar datas em Swift, como usando a classe `Calendar` ou a biblioteca `DateTools`, que oferecem mais opções e funcionalidades.

Além da comparação entre duas datas, também é possível calcular a diferença entre elas, usando métodos como `timeIntervalSince` ou `timeTo`. Essas informações podem ser úteis em situações em que se deseja saber quanto tempo passou desde uma determinada data.

Para implementar a comparação entre datas, o Swift utiliza o padrão internacional ISO 8601. Isso garante uma comparação precisa e consistente em qualquer dispositivo ou sistema operacional.

## Veja também:
- [Documentação oficial do Swift sobre Comparing Dates](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Artigo da Ray Wenderlich sobre Comparing Dates in Swift](https://www.raywenderlich.com/900-swift-date-manipulation-nsdateformatter-and-nscalendar)
- [Biblioteca DateTools para Swift](https://github.com/MatthewYork/DateTools)