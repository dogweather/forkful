---
title:                "Swift: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao trabalhar em um projeto de programação, pode ser necessário remover caracteres específicos de uma string que correspondam a um determinado padrão. Isso pode ser útil para limpar os dados ou formatar uma entrada de usuário, por exemplo.

## Como fazer

Existem várias maneiras de remover caracteres que correspondam a um padrão em Swift. Uma maneira simples de fazer isso é usando o método `replacingOccurrences(of:with:)` em uma string, onde o primeiro argumento é o padrão a ser encontrado e o segundo é a string de substituição. Por exemplo:

```Swift
let sentence = "Eu amo programação em Swift!"
let newSentence = sentence.replacingOccurrences(of: "am", with: "gosto")
print(newSentence)
// Output: "Eu gosto programação em Swift!"
```

Outra opção é usar expressões regulares com a classe `NSRegularExpression`. Esta é uma maneira mais avançada de lidar com padrões e oferece uma maior flexibilidade. Por exemplo:

```Swift
let sentence = "Eu amo programação em Swift!"
let regex = try! NSRegularExpression(pattern: "\\w*amo\\w*", options: [])
let newSentence = regex.stringByReplacingMatches(in: sentence, options: [], range: NSRange(location: 0, length: sentence.utf16.count), withTemplate: "gosto")
print(newSentence)
// Output: Eu gosto programação em Swift!
```

## Deep Dive

Ao usar o método `replacingOccurrences(of:with:)`, é importante notar que ele faz distinção entre maiúsculas e minúsculas. Isso significa que ao usar este método para substituir caracteres, ele só substituirá aqueles que correspondem exatamente ao padrão fornecido.

Já ao usar expressões regulares, é possível usar símbolos especiais para definir regras mais complexas e substituir partes específicas de uma string. Além disso, a classe `NSRegularExpression` oferece muitos métodos úteis para o trabalho com expressões regulares, como verificar se a string corresponde ao padrão ou obter informações sobre as correspondências encontradas.

## Veja também

- [Documentação oficial do Swift: NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tutorial de expressões regulares em Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)