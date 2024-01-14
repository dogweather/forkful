---
title:                "Swift: Lendo argumentos de linha de comando"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Antes de mergulharmos em como ler argumentos de linha de comando em Swift, é importante entender porque isso pode ser útil. Ao ler argumentos de linha de comando, um programa pode ser executado de maneira diferente dependendo do que é fornecido pelo usuário. Isso permite uma maior personalização e controle sobre o funcionamento do programa.

## Como Fazer

Para ler argumentos de linha de comando em Swift, primeiro é necessário importar o módulo "Foundation". Em seguida, podemos utilizar a propriedade "CommandLine.arguments" para acessar uma matriz contendo todos os argumentos fornecidos pelo usuário. Por exemplo:

```Swift 
import Foundation 

let arguments = CommandLine.arguments 
print(arguments) 
``` 

Ao executar o programa acima e fornecer alguns argumentos na linha de comando, como "Hello World", a saída será ["Hello", "World"]. Isso significa que o primeiro argumento será armazenado na posição 0 da matriz e o segundo argumento na posição 1.

## Mergulho Profundo

Além de acessar os argumentos individualmente, também podemos usar o método "index(after:)" para acessar o próximo argumento em uma determinada posição. Além disso, podemos usar o método "hasPrefix(_ prefix: String)" para verificar se um argumento começa com um determinado prefixo. Isso pode ser útil para criar diferentes funcionalidades com base nos argumentos fornecidos pelo usuário.

## Veja Também

- [Documentação oficial da linguagem Swift sobre os argumentos de linha de comando](https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID521)
- [Tutorial sobre como ler argumentos de linha de comando em Swift](https://www.raywenderlich.com/163134/command-line-programs-macos-tutorial-2)
- [Exemplos práticos de leitura de argumentos em linha de comando em Swift](https://nshipster.com/arguments/)