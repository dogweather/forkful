---
title:                "Swift: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Se você é novo no mundo da programação, pode se perguntar por que alguém se preocuparia em encontrar o comprimento de uma string. Afinal, isso parece ser uma tarefa simples. No entanto, saber como encontrar o comprimento de uma string é uma habilidade fundamental em qualquer linguagem de programação, incluindo Swift. Isso pode ser útil ao construir aplicativos que requerem a manipulação de dados de texto, como formulários ou mensagens de usuário.

## Como

Agora vamos mergulhar no código Swift para descobrir como encontrar o comprimento de uma string. Primeiro, precisamos definir uma string usando aspas duplas, como neste exemplo:

``` Swift
let minhaString = "Olá, mundo!"
```

Em seguida, usamos o método `count` para encontrar o número de caracteres na string:

``` Swift
let comprimento = minhaString.count
```

Então, basta imprimir o resultado para ver o comprimento da sua string:

``` Swift
print(comprimento)
```

O console irá mostrar: `12`, que é o número de caracteres em "Olá, mundo!".

## Mergulho profundo

Agora que sabemos como encontrar o comprimento de uma string, vamos entender melhor como funciona. O método `count` retorna o número de caracteres na string, incluindo espaços, pontuação e caracteres especiais. O comprimento de uma string vazia será 0.

É importante lembrar que, no Swift, cada caractere tem um valor numérico atribuído a ele. Por exemplo, a letra "a" tem um valor de 97. Então, ao usar o método `count`, ele conta quantos caracteres individuais estão presente na string e não o número de palavras.

## Veja também

Aqui estão alguns links úteis para expandir seu conhecimento sobre strings e outras funcionalidades do Swift:

- [Documentação Oficial do Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Ray Wenderlich - Strings no Swift](https://www.raywenderlich.com/3967712-swift-strings-and-characters-getting-started)
- [Hacking with Swift - Manipulação de Strings](https://www.hackingwithswift.com/quick-start/understanding-swift/manipulating-strings)

Agora você deve estar pronto para trabalhar com strings em seu próximo projeto Swift! Pratique e você se tornará um mestre em pouco tempo. Divirta-se programando!