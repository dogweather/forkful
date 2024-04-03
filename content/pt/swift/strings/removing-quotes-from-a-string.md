---
date: 2024-01-26 03:42:30.115072-07:00
description: "Como fazer: Swift permite que voc\xEA realize a tarefa de remo\xE7\xE3\
  o de aspas de maneira bastante pr\xE1tica. Aqui est\xE1 um exemplo r\xE1pido usando\u2026"
lastmod: '2024-03-13T22:44:46.909170-06:00'
model: gpt-4-0125-preview
summary: "Swift permite que voc\xEA realize a tarefa de remo\xE7\xE3o de aspas de\
  \ maneira bastante pr\xE1tica."
title: Removendo aspas de uma string
weight: 9
---

## Como fazer:
Swift permite que você realize a tarefa de remoção de aspas de maneira bastante prática. Aqui está um exemplo rápido usando `replacingOccurrences(of:with:)`, que faz exatamente o que parece—substitui pedaços de texto por outra coisa, ou por nada.

```swift
var quotedString = "\"This is a 'quoted' string.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // This is a 'quoted' string.

// Lidando com aspas simples? Basta mudar o termo de busca.
quotedString = "'Here's another example.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Heres another example.
```

O resultado serão strings sem aspas, prontas para o que você planejou a seguir.

## Mergulho Profundo
Estamos "limpando" strings como essas desde o amanhecer da programação. Nos primeiros dias, era mais sobre conservar memória valiosa e evitar erros de sintaxe no processamento de entradas. Avançando para os dias de hoje, trata-se de boa higiene de dados—especialmente ao lidar com JSON ou preparando strings para trabalho em banco de dados. Uma aspa perdida pode jogar uma chave inglesa em consultas SQL mais rápido do que você pode dizer "erro de sintaxe."

Alternativas? Bem, se você achar que `replacingOccurrences(of:with:)` é um pouco simples demais, você pode mergulhar em expressões regulares para padrões mais complexos ou quando desejar remover aspas apenas em certas posições. A classe `NSRegularExpression` do Swift é sua amiga aqui. Mas lembre-se, regex pode ser uma espada de dois gumes—poderosa, mas às vezes excessiva.

Em termos de implementação, `replacingOccurrences(of:with:)` é um método fornecido por `String` no Swift, o qual internamente chama funções de manipulação de strings mais complexas que lidam com Unicode e outras intrincadas do processamento de texto moderno. É uma daquelas coisas "simples por fora, complexas por dentro" que o Swift lida para que você não precise.

## Veja Também
Para mais sobre manipulações de string em Swift:

- The Swift Programming Language (Strings and Characters): [Documentação Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Documentação para Desenvolvedores da Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)

E se agora você está curioso sobre expressões regulares e quer testar seus padrões:

- Regex101: [Testador e Debugger de Regex](https://regex101.com)
