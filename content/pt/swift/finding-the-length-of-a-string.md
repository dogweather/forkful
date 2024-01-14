---
title:    "Swift: Encontrando o tamanho de uma string."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Você já se perguntou como é possível descobrir o tamanho de uma string no Swift? Saber o comprimento de uma string é uma habilidade fundamental em programação e pode ser especialmente útil ao trabalhar com textos e manipulação de dados. Neste post, vamos explicar como encontrar o comprimento de uma string no Swift e por que isso é importante.

## How To
Para encontrar o comprimento de uma string no Swift, podemos usar a propriedade `count` da classe `String`. Vamos dar uma olhada em um exemplo de código:

```Swift 
let minhaString = "Olá, mundo!"
print(minhaString.count)

// Output: 12
```

Neste exemplo, criamos uma variável chamada `minhaString` e atribuímos a ela o valor de "Olá, mundo!". Em seguida, usamos o método `count` para encontrar o comprimento dessa string e imprimimos o resultado. Como a string possui 12 caracteres (incluindo espaços), o output será 12.

Além disso, é importante lembrar que o método `count` também pode ser usado com strings contendo emojis, caracteres acentuados e outros símbolos especiais.

```Swift
let minhaString = "👋 Olá, mundo! 😊"
print(minhaString.count)

// Output: 17
```

## Deep Dive
Então, como o método `count` funciona para encontrar o comprimento de uma string no Swift? Na verdade, ele funciona de maneira bastante simples: contando o número de caracteres na string. Isso inclui letras, números, espaços, emojis e outros símbolos. Quando usamos o método `count`, não precisamos nos preocupar com a codificação ou a contagem de bytes, pois ele lida com isso automaticamente.

Outra coisa importante a ser observada é que o método `count` considera cada caractere individualmente, independentemente do seu tamanho. Por exemplo, o caractere "é" seria contado como um único caractere, mesmo que seja composto por duas letras.

## Veja também
- [Documentação oficial da classe String do Swift](https://developer.apple.com/documentation/swift/string)
- [Tutorial de strings no Swift](https://www.hackingwithswift.com/syntax/0-strings)

Esperamos que este post tenha sido útil para você entender como encontrar o comprimento de uma string no Swift. Agora, você pode aplicar essa habilidade em seus projetos e aprimorar suas habilidades de programação. Até a próxima!