---
title:                "Buscando e substituindo texto"
html_title:           "Swift: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Você já se viu na situação de precisar mudar todas as instâncias de uma palavra ou frase em um código? Sabemos que fazer isso manualmente pode ser um processo tedioso e propenso a erros. Felizmente, no Swift, existem algumas formas eficientes de realizar essa tarefa de busca e substituição de texto.

## Como Fazer

Existem duas maneiras principais de fazer busca e substituição de texto no Swift: usando o método `replacingOccurrences()` ou a sintaxe de expressões regulares.

Para usar o método `replacingOccurrences()`, basta informar a palavra ou frase que você deseja substituir e a nova palavra ou frase a ser utilizada. Por exemplo:

```Swift
let texto = "Eu gosto de programar em Swift."
let novoTexto = texto.replacingOccurrences(of: "gosto de", with: "amo")
print(novoTexto)

// Saída: Eu amo programar em Swift.
```

Já para usar expressões regulares, é necessário importar a biblioteca `NSRegularExpression`. Por exemplo, para substituir todas as letras maiúsculas por minúsculas em um texto, podemos fazer o seguinte:

```Swift
import Foundation

let texto = "Hello World!"
let padrao = try! NSRegularExpression(pattern: "[A-Z]")
let novoTexto = padrao.stringByReplacingMatches(in: texto, options: [], range: NSRange(0, texto.utf16.count), withTemplate: "a")
print(novoTexto)

// Saída: hello world!
```

## Mergulho Profundo

Além das opções mencionadas acima, o Swift também oferece a função `range(of:)`, que pode ser usada para localizar a posição de uma palavra ou frase específica em uma string. Este método retorna um objeto `Range` que contém o início e o fim da correspondência encontrada.

Outra opção é o uso de expressões regulares com o método `matches(in:options:range:completionHandler:)`, que permite encontrar todas as correspondências em uma string e manipulá-las de forma personalizada.

## Veja Também

Se você quiser aprofundar seus conhecimentos em busca e substituição de texto no Swift, recomendamos a leitura dos seguintes recursos:

- [Documentação oficial do Swift sobre busca e substituição de texto](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID357)
- [Tutorial de busca e substituição de texto no Swift](https://learnappmaking.com/swift-search-and-replace-text-nsregularexpression/)
- [Vídeo tutorial sobre expressões regulares no Swift](https://www.youtube.com/watch?v=Zg8Ch6FN1jo)