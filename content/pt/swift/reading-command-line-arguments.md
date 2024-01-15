---
title:                "Lendo argumentos da linha de comando"
html_title:           "Swift: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Ler argumentos de linha de comando pode ser útil quando se está criando um programa que precisa de interação com o usuário em tempo de execução. Além disso, utilizar argumentos de linha de comando pode tornar o seu código mais flexível e adaptável a diferentes situações.

## Como fazer

Para ler argumentos de linha de comando em Swift, você precisará do objeto `CommandLine`. É possível acessar os argumentos passados pelo usuário através da propriedade `.arguments` deste objeto. Veja um exemplo prático:

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

Se o usuário passar argumentos na execução do seu programa, você verá eles sendo impressos no console. Por exemplo, se você executar o programa com o comando `swift meuPrograma.swift argumento1 argumento2`, o output será:

```
["meuPrograma.swift", "argumento1", "argumento2"]
```

## Aprofundando

Além de simplesmente acessar os argumentos passados, existem outras funcionalidades úteis oferecidas pelo objeto `CommandLine`. Por exemplo, se você quiser acessar o caminho para o diretório atual, pode utilizar a propriedade `.currentDirectoryPath`. Já a propriedade `.executableURL` permite que você acesse o caminho para o executável do seu programa.

Outra funcionalidade interessante é a possibilidade de passar opções na linha de comando e acessá-las através do método `.hasOption()`. Por exemplo, se você executar o comando `swift meuPrograma.swift -s`, o método `.hasOption("-s")` retornará `true`. Isso pode ser útil para adicionar diferentes comportamentos ao seu programa dependendo das opções selecionadas pelo usuário.

## Veja também

- [Documentação oficial do CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Leitura de argumentos de linha de comando em Swift (em inglês)](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-command-line-parser)