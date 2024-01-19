---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Procurar e Substituir Texto em Swift

## O Que e Por Que?

Procurar e substituir texto é uma operação comum em programação que lida com a modificação de strings. Programadores fazem isso para realizar operações como correção de erros de digitação, reformatar dados de texto e outros cenários que exigem a alteração de partes específicas de uma string.

## Como Fazer:

Aqui está um exemplo simples de como procurar e substituir texto em Swift usando o método `replacingOccurrences`.

```Swift
let texto = "Olá, mundo!"
let novoTexto = texto.replacingOccurrences(of: "mundo", with: "Swift")
print(novoTexto)
```

Saída:

```Swift
Olá, Swift!
```
Nesse caso, substituímos a palavra "mundo" por "Swift".

## Mergulho Profundo

Historicamente, a necessidade de substituir texto levou à invenção de expressões regulares, que são uma maneira poderosa e flexível de procurar e editar texto. Em Swift, você também pode usar expressões regulares para procurar e substituir texto, embora possa ser mais complexo.

Há outras maneiras de realizar a tarefa de substituição de texto. Por exemplo, você pode dividir a string em um array de substrings, fazer a substituição nas partes desejadas e, em seguida, juntar tudo novamente. No entanto, o método `replacingOccurrences` é provavelmente o mais direto.

Na implementação real, o método `replacingOccurrences` de Swift realiza uma pesquisa completa do texto alvo, coletando todas as posições de início dos textos de busca antes de fazer qualquer substituição. Isso significa que é segura para substituições que alteram o número de caracteres na string.

## Veja Também

- Documentação da Apple sobre Strings e Textos em Swift: [Apple Docs](https://developer.apple.com/documentation/swift/string)
- Artigo: [Working with Strings in Swift](https://www.hackingwithswift.com/articles/141/8-powerful-swift-features-that-were-stolen-from-other-languages)
- Guia: [Swift Standard Library String Reference](https://developer.apple.com/documentation/swift/string)