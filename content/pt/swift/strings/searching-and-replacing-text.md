---
date: 2024-01-20 17:58:46.043728-07:00
description: 'Como fazer: Segue um exemplo simples de como procurar e substituir texto
  em Swift.'
lastmod: '2024-03-13T22:44:46.906517-06:00'
model: gpt-4-1106-preview
summary: Segue um exemplo simples de como procurar e substituir texto em Swift.
title: Pesquisando e substituindo texto
weight: 10
---

## Como fazer:
Segue um exemplo simples de como procurar e substituir texto em Swift:

```swift
let textoOriginal = "Olá, mundo! Programar é top!"
let textoProcurado = "mundo"
let substituicao = "universo"

let textoAtualizado = textoOriginal.replacingOccurrences(of: textoProcurado, with: substituicao)

print(textoAtualizado)
```

Output:

```
Olá, universo! Programar é top!
```

## Mergulho Profundo
Historicamente, a habilidade de procurar e substituir texto remonta às primeiras interfaces de editores de texto, como o VI e o Emacs. Em Swift e outras linguagens modernas, há diversas formas de realizar essa operação - você pode usar simples métodos como `replacingOccurrences`, ou até expressões regulares (Regex) para padrões mais complexos. A implementação em Swift é eficiente, mas vale sempre ficar de olho em casos de uso com grandes quantidades de texto, onde o desempenho pode ser um fator crítico.

Alternativas de implementação incluem o uso da classe `NSMutableString` ou frameworks como o `NSRegularExpression` para casos mais avançados onde mais controle é necessário. A escolha entre essas opções depende do seu caso específico e dos requisitos de desempenho.

## Veja Também
- Apple Swift Documentation: [Text and Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Tutorial em vídeo sobre expressões regulares em Swift: [Swift Regex Tutorial](https://www.raywenderlich.com/2292-regular-expressions-tutorial-getting-started)
- Documentação da Apple sobre `NSRegularExpression`: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
