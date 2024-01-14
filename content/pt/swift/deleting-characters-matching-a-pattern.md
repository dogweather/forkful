---
title:    "Swift: Excluindo caracteres que correspondam a um padrão"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

A exclusão de caracteres que correspondem a um padrão pode ser útil em várias situações de programação, como por exemplo, manipulação de string e processamento de dados.

## Como fazer

Para deletar caracteres que correspondem a um padrão no Swift, podemos usar a função `replacingOccurrences` da classe `String`. Veja um exemplo abaixo:

```Swift
let minhaString = "Olá, mundo!"
let novaString = minhaString.replacingOccurrences(of: "mundo", with: "")
print(novaString) // Saída: Olá,!
```

Neste exemplo, usamos a função `replacingOccurrences` para substituir a palavra "mundo" por uma string vazia, resultando na exclusão dessa palavra da string original.

## Aprofundando

Essa função pode receber diferentes parâmetros, permitindo assim uma maior flexibilidade na exclusão de caracteres. Além disso, é possível usar expressões regulares para especificar o padrão a ser excluído. As expressões regulares são uma forma poderosa de lidar com padrões de texto e podem ser muito úteis na exclusão de caracteres.

## Veja também

- [Documentação oficial do Swift sobre a função `replacingOccurrences`](https://developer.apple.com/documentation/foundation/nsstring/1416386-replacingoccurrences)
- [Guia de expressões regulares em Swift](https://www.raywenderlich.com/661-regex-tutorial-swift-getting-started)
- [Documentação do Swift sobre expressões regulares](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID434)