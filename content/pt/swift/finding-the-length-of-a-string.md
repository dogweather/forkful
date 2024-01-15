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

## Por que

Você já se perguntou como os seus aplicativos favoritos conseguem calcular o número de caracteres em uma string? Saber como encontrar o comprimento de uma string é uma habilidade essencial para qualquer programador Swift.

## Como fazer

Encontrar o comprimento de uma string é simples e pode ser feito de várias maneiras. Vamos dar uma olhada em algumas opções:

```Swift
let string = "Hello, world!"
let length = string.count
print(length) //Output: 13
```

Neste exemplo, usamos o método built-in `count` para encontrar o comprimento da string "Hello, world!". O resultado é armazenado na variável `length` e depois impresso na console.

Também podemos usar o método `characters.count` para encontrar o comprimento de uma string:

```Swift
let string = "Hello, world!"
let length = string.characters.count
print(length) //Output: 13
```

Este método é semelhante ao `count`, mas é útil quando queremos encontrar o número de caracteres em uma string que contém símbolos especiais ou emojis.

Outra opção é usar um loop `for` para iterar por cada caractere da string e adicionar 1 ao comprimento a cada iteração:

```Swift
let string = "Hello, world!"
var length = 0

for _ in string {
    length += 1
}

print(length) //Output: 13
```

Embora esta opção seja um pouco mais verbosa, é útil se quisermos adicionar alguma lógica adicional ao nosso código.

## Mergulho Profundo

Você pode estar se perguntando por que existem duas maneiras de encontrar o comprimento de uma string (`count` e `characters.count`). Isso ocorre porque, em Swift 4, os caracteres individuais em uma string não são mais considerados como coleção de caracteres. Em vez disso, uma string é considerada como uma coleção de caracteres em qualquer formato, incluindo símbolos especiais e emojis, e isso exigiu a introdução do método `characters.count`.

Além disso, se você estiver interessado em compreender melhor como o método `count` funciona, ele usa o `UTF16View`, que é uma representação em UTF-16 de uma string. Isso significa que o método `count` não leva em conta o número de bytes em uma string, mas sim o número de elementos em sua representação UTF-16.

## Veja também

- [Guia da Apple sobre strings em Swift](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html)
- [Documentação oficial da Apple sobre a propriedade `count`](https://developer.apple.com/documentation/swift/string/2894481-count)