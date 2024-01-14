---
title:    "Swift: Lendo argumentos da linha de comando"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Swift?

Ler argumentos da linha de comando é uma habilidade essencial para qualquer programador Swift, especialmente para aqueles que desejam criar aplicativos para linha de comando. Ao entender como ler e processar argumentos da linha de comando, você poderá criar programas mais dinâmicos e interativos.

## Como fazer:

Para ler argumentos da linha de comando em Swift, você precisará usar a classe `CommandLine` do framework `Foundation`. Esta classe fornece métodos e propriedades para acessar os argumentos de entrada fornecidos pela linha de comando.

Para começar, importe o framework `Foundation` no início do seu arquivo Swift:

```Swift
import Foundation
```

Em seguida, você pode acessar os argumentos da linha de comando usando a propriedade `CommandLine.arguments`:

```Swift
let arguments = CommandLine.arguments
```

Esta propriedade retorna um array de strings, onde cada elemento representa um dos argumentos fornecidos na linha de comando. Você também pode acessar argumentos específicos usando seus índices, começando do primeiro argumento após o nome do programa.

Aqui está um exemplo de código que lê argumentos da linha de comando e imprime-os na tela:

```Swift
// Input: swift hello.swift John 25
// Output: Hello John, you are 25 years old!

import Foundation

let arguments = CommandLine.arguments

// Primeiro argumento é o nome do programa, então começamos do segundo argumento
let name = arguments[1]
let age = arguments[2]

print("Hello \(name), you are \(age) years old!")
```

Execute este código, passando seu nome e idade como argumentos da linha de comando, e veja o resultado!

## Mergulho profundo:

A classe `CommandLine` também fornece métodos para analisar argumentos da linha de comando de maneira mais avançada. Por exemplo, você pode usar o método `option` para definir opções opcionais e seus respectivos valores.

Além disso, você também pode usar a biblioteca Swift Argument Parser, um pacote de terceiros popular para lidar com argumentos da linha de comando de forma ainda mais eficiente.

## Veja também:

- [Documentação da classe `CommandLine`](https://developer.apple.com/documentation/foundation/commandline)
- [Swift Argument Parser](https://github.com/apple/swift-argument-parser)