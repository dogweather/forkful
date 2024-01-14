---
title:    "Swift: Utilizando expressões regulares"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares (Regular Expressions)?

Expressões regulares são uma ferramenta poderosa e versátil na programação em Swift, permitindo que você procure padrões específicos em strings e faça alterações dinâmicas. Elas podem economizar uma quantidade significativa de tempo e esforço em projetos de programação, tornando-as uma habilidade valiosa para dominar.

## Como usar Expressões Regulares em Swift

Para começar a usar expressões regulares em Swift, você precisa importar a biblioteca padrão de expressões regulares e especificar o padrão que deseja procurar. Por exemplo, se você quiser encontrar todas as ocorrências de "Swift" em uma string, você pode fazer isso da seguinte maneira:

```Swift
import Foundation

let str = "A programação em Swift é incrível! Eu amo programar em Swift."

let regex = try! NSRegularExpression(pattern: "Swift", options: [])

let matches = regex.matches(in: str, options: [], range: NSRange(location: 0, length: str.utf16.count))

for match in matches {
    print("Encontrada uma ocorrência de \"Swift\" na posição: \(match.range.location)")
}

```

Isso retornará a seguinte saída:

```
Encontrada uma ocorrência de "Swift" na posição: 21
Encontrada uma ocorrência de "Swift" na posição: 46
```

## Profundidade sobre o uso de Expressões Regulares

Além de simplesmente encontrar padrões em strings, expressões regulares têm complexidade e utilidade muito maiores. Elas podem ser usadas para validar formatos de dados, substituir caracteres indesejados e até mesmo para dividir strings em componentes úteis.

Por exemplo, você pode usar uma expressão regular para verificar se um número de telefone fornecido pelo usuário está em um formato válido:

```Swift
let phoneNumber = "123-456-7890"

let regex = try! NSRegularExpression(pattern: "[0-9]{3}-[0-9]{3}-[0-9]{4}", options: [])

let matches = regex.matches(in: phoneNumber, options: [], range: NSRange(location: 0, length: phoneNumber.utf16.count))

if matches.count > 0 {
    print("Número de telefone válido!")
} else {
    print("Número de telefone inválido!")
}
```

Isso retornará a seguinte saída:

```
Número de telefone válido!
```

## Veja também

Aqui estão algumas referências adicionais para ajudá-lo a se aprofundar no uso de expressões regulares em suas aplicações Swift:

- [Documentação oficial da Apple para `NSRegularExpression`](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tutorial da Ray Wenderlich sobre como usar expressões regulares em Swift](https://www.raywenderlich.com/1948484-regular-expressions-tutorial-in-swift-getting-started)
- [Livro "Regular Expressions Cookbook" de Jan Goyvaerts e Steven Levithan](https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/)