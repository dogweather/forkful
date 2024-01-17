---
title:                "Exclusão de caracteres que correspondem a um padrão"
html_title:           "Swift: Exclusão de caracteres que correspondem a um padrão"
simple_title:         "Exclusão de caracteres que correspondem a um padrão"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que e Por que?

Excluir caracteres que correspondem a um padrão é uma técnica comum usada pelos programadores para manipular e limpar dados em suas aplicações. Isso permite que eles filtrem e removam informações indesejadas de uma maneira rápida e eficiente.

## Como Fazer:

Para excluir caracteres que correspondem a um padrão em Swift, você pode usar a função ```replacingOccurrences``` ou a propriedade ```filtered``` para strings. Por exemplo, se quisermos remover todas as vogais de uma string, podemos fazer o seguinte:

```swift
let string = "Esta é uma frase com vogais."

let semVogais = string.replacingOccurrences(of: "[aeiou]", with: "", options: .regularExpression, range: nil)

print(semVogais) // St frs cm vgl.
```

## Mergulho Profundo:

A prática de excluir caracteres que correspondem a um padrão existe há décadas e é comumente usada em linguagens de programação para manipular strings. Alternativas para essa técnica incluem o uso de expressões regulares e laços de repetição para percorrer e modificar a string manualmente.

No Swift, a função ```replacingOccurrences``` e a propriedade ```filtered``` foram introduzidas na versão 4.0 da linguagem, tornando a manipulação de strings ainda mais simples e poderosa.

## Veja Também:

Para mais informações sobre a função ```replacingOccurrences``` e a propriedade ```filtered```, consulte a documentação oficial do Swift em https://developer.apple.com/documentation/swift/string/.

Para aprender mais sobre expressões regulares, confira este tutorial da Ray Wenderlich em https://www.raywenderlich.com/1425-regular-expressions-tutorial-getting-started.

E para descobrir outras maneiras de manipular strings em Swift, dê uma olhada neste artigo do Hacking with Swift em https://www.hackingwithswift.com/articles/141/8-useful-swift-extensions.