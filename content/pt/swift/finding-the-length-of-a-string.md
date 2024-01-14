---
title:    "Swift: Encontrando o comprimento de uma string"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa básica em programação que pode ser usada em uma variedade de aplicativos, desde manipulação de dados até validação de entradas de usuário. É uma habilidade fundamental que todo programador Swift deve ter em seu arsenal.

## Como fazer

Para encontrar o comprimento de uma string em Swift, podemos usar a propriedade `count` da string. Vamos dar uma olhada em um exemplo simples em que declaramos uma string e, em seguida, imprimimos seu comprimento usando a propriedade `count`:

```Swift
let minhaString = "Olá mundo!"
print(minhaString.count) // Output: 11
```

Podemos até mesmo usar `count` em strings contendo emojis ou caracteres especiais, e ele nos dará o comprimento correto da string:

```Swift
let minhaStringEmoji = "😀😎🚀"
print(minhaStringEmoji.count) // Output: 3
```

## Mergulho Profundo

Embora a propriedade `count` seja uma forma simples e eficaz de encontrar o comprimento de uma string, é importante ter em mente que ela conta o número de caracteres individuais na string, não o número de palavras. Portanto, se tivermos uma string com espaços em branco, os espaços também serão contados no comprimento da string. Para contabilizar o número de palavras em uma string, podemos usar o método `components(separatedBy:)` e, em seguida, contar o número de itens no array resultante.

Outro ponto importante a considerar é que, dependendo do idioma usado na string, o `count` pode não ser igual ao número de caracteres visíveis. Por exemplo, letras acentuadas em um idioma como o português são representadas por mais de um byte, então, se a propriedade `count` for usada em uma string com acentos, o número retornado não será o número de caracteres visíveis na string.

## Veja também

- [Documentação oficial do Swift sobre a propriedade `count`](https://developer.apple.com/documentation/swift/string/2995066-count)
- [Como encontrar o número de palavras em uma string em Swift](https://www.hackingwithswift.com/example-code/strings/how-to-count-the-number-of-words-in-a-string)
- [Manipulando strings com espaços em branco em Swift](https://www.hackingwithswift.com/example-code/strings/how-to-remove-extra-whitespace-characters)