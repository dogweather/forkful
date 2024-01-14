---
title:    "Swift: Comparando duas datas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Swift?

Comparar duas datas é uma tarefa comum na programação, principalmente em aplicativos que lidam com eventos, agendamentos e prazos. Ao comparar duas datas, é possível determinar qual delas é mais recente ou se elas são iguais, o que pode ser útil em diversas situações.

## Como comparar duas datas em Swift

Comparar duas datas em Swift é relativamente simples, pois a linguagem possui uma classe chamada `Date` que permite a manipulação de datas. Vamos ver um exemplo de como comparar duas datas usando a biblioteca padrão do Swift:

```
let data1 = Date() // Data atual
let data2 = Date(timeIntervalSinceNow: 86400) // Data com um dia de diferença
if data1 < data2 {
  print("data1 é mais antiga que data2")
} else if data2 < data1 {
  print("data2 é mais antiga que data1")
} else {
  print("as datas são iguais")
}
```

Nesse exemplo, criamos duas datas: `data1`, que é a data atual, e `data2`, que é a data de amanhã. Usando o operador de comparação `<`, podemos verificar qual das duas datas é mais antiga. Se a condição for verdadeira, imprimimos uma mensagem indicando isso. Caso contrário, verificamos se `data2` é mais antiga que `data1` e imprimimos a mensagem correspondente. Se nenhuma das condições for verdadeira, quer dizer que as datas são iguais e imprimimos uma mensagem informando isso.

## Mergulho Profundo

Além do operador de comparação `<`, a classe `Date` possui outros métodos que podem ser úteis para comparação de datas, como `isEqual(to:)` e `compare(to:)`. Além disso, é possível especificar com mais detalhes o tempo que está sendo comparado, como somente a data ou somente a hora. É importante lembrar também que datas são representadas em formato UTC no Swift, o que pode causar algumas diferenças em relação ao fuso horário local.

## Veja também

- [Documentação oficial do Swift sobre data e tempo](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial de como trabalhar com datas em Swift](https://www.ralfebert.de/snippets/ios/swift-dates/)