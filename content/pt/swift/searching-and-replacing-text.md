---
title:    "Swift: Busca e substituição de texto"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que utilizar a função de busca e substituição de texto?

A função de busca e substituição de texto é uma ferramenta essencial para programadores no processo de edição e manipulação de strings. Ela permite que sejam feitas alterações precisas e rápidas em um grande volume de texto, economizando tempo e recursos.

## Como utilizar a função de busca e substituição de texto em Swift

```Swift
let texto = "Aprender programação é muito divertido!"
let novoTexto = texto.replacingOccurrences(of: "divertido", with: "desafiador")

print(novoTexto)

// Output: Aprender programação é muito desafiador!
```

A função `replacingOccurrences(of:with:)` recebe dois parâmetros: o texto a ser substituído e o novo texto a ser utilizado. É importante ressaltar que ela não altera a string original, mas sim retorna uma nova string com as alterações feitas.

## Aprofundando na função de busca e substituição de texto

Além de definir um texto específico para ser substituído, é possível utilizar expressões regulares na função `replacingOccurrences(of:with:)`, o que permite fazer substituições ainda mais complexas. Por exemplo:

```Swift
let texto = "Maria e João eram grandes amigos."

let novoTexto = texto.replacingOccurrences(of: "[Mm]aria", with: "José", options: .regularExpression, range: nil)

print(novoTexto)

// Output: José e João eram grandes amigos.
```

No exemplo acima, a expressão regular `[Mm]aria` identifica tanto "Maria" como "maria", substituindo ambos por "José". Além disso, é possível limitar a substituição a uma determinada parte da string, definindo um range no último parâmetro.

## Veja também

- [Documentação oficial da função replacingOccurrences (em inglês)](https://developer.apple.com/documentation/swift/string/2427941-replacingoccurrences)
- [Tutorial de expressões regulares em Swift (em inglês)](https://www.raywenderlich.com/5767-regular-expressions-tutorial-getting-started)
- [Guia completo de programação em Swift (em português)](https://www.devmedia.com.br/curso/desenvolvimento-mobile-ios-com-swift-parte-1/1477)