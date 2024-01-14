---
title:    "Swift: Concatenando strings"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
Concatenar strings é uma habilidade fundamental em programação Swift. Isso permite que você junte várias strings em uma única, facilitando a formatação e apresentação de dados em seus aplicativos.

## Como Fazer
``` Swift
let firstName = "João"
let lastName = "Silva"

let fullName = firstName + " " + lastName

print(fullName)

// Output: João Silva
```

``` Swift
let age = 25
let yearsOld = "anos"

let description = "Eu tenho" + " " + String(age) + " " + yearsOld

print(description)

// Output: Eu tenho 25 anos
```

Aqui, você pode ver que podemos usar o operador de adição (+) para concatenar strings. Além disso, podemos usar o tipo de dados "String" para converter outros tipos de dados, como inteiros, em strings.

## Mergulho Profundo
Existem outras maneiras de concatenar strings, como usar a função "String Interpolation", que permite que você insira valores de variáveis diretamente na string. Por exemplo:

``` Swift
let name = "Maria"
let age = 30

let greeting = "Olá \(name), você tem \(age) anos."

print(greeting)

// Output: Olá Maria, você tem 30 anos.
```

Além disso, é importante ter cuidado com o desempenho ao concatenar grandes quantidades de strings. Em vez de usar o operador de adição (+) repetidamente, é melhor usar a função "String Interpolation" ou a classe "StringBuilder" em situações onde você precisa concatenar muitas strings.

## Veja também
- [Documentação oficial da Apple sobre strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial de concatenar strings em Swift](https://www.appcoda.com/string-interpolation-concatenation-swift/)
- [Dicas de desempenho em concatenação de strings](https://www.hackingwithswift.com/articles/162/how-to-build-speedy-swift-strings-using-stringinterpolation)