---
title:                "Encontrando o comprimento de uma string"
html_title:           "Swift: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que é importante?

Encontrar o comprimento ou tamanho de uma string é uma tarefa comum e importante para os programadores em Swift. O comprimento de uma string é simplesmente a quantidade de caracteres que ela contém, incluindo espaços, símbolos e números. Saber o tamanho de uma string é útil para muitas operações, como formatar saídas de dados ou validar entradas do usuário.

## Como fazer:

Para encontrar o comprimento de uma string em Swift, podemos usar o método `count` na própria string. Veja um exemplo:

```Swift
let minhaString = "Olá mundo!"
let tamanho = minhaString.count
print("O tamanho da string é \(tamanho).") // Output: O tamanho da string é 12.
```

Como podemos ver, o tamanho da string é retornado como um valor numérico que podemos usar em outras partes do código.

## Aprofundando:

Encontrar o comprimento de uma string é algo que os programadores têm feito há décadas em diferentes linguagens de programação. No entanto, a maneira como isso é feito pode variar de linguagem para linguagem. Em Swift, usar o método `count` é a maneira mais eficiente e direta de encontrar o tamanho de uma string. Uma alternativa seria usar um laço de repetição para iterar por cada caractere da string e contar manualmente.

## Veja também:

- Documentação oficial da Apple sobre strings em Swift: https://developer.apple.com/documentation/swift/string
- Tutorial interativo sobre strings em Swift: https://www.hackingwithswift.com/quick-start/strings/introduction