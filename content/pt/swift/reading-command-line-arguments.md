---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O quê e por quê?

A leitura de argumentos da linha de comando consiste em acessar os dados inseridos em um determinado comando no console. Programadores utilizam isso para controlar como o programa irá se comportar baseado nesses argumentos.

## Como fazer:

Aqui está um exemplo de como ler argumentos da linha de comando:

```Swift
import Foundation

//Acessa os argumentos da linha de comando
let arguments = CommandLine.arguments

print("Há \(arguments.count) argumentos")

for (index, argument) in arguments.enumerated() {
    print("Argumento \(index): \(argument)")
}
```

Se você executar esse programa com o seguinte comando: `swift programa.swift primeiro_argumento segundo_argumento`, terá essa saída:

```
Há 3 argumentos
Argumento 0: ./programa.swift
Argumento 1: primeiro_argumento
Argumento 2: segundo_argumento
```

## Detalhando:

Historicamente, argumentos de linha de comando têm sido uma maneira efetiva de dar instruções a um programa. Eles são normalmente usados para iniciar um programa com configurações específicas.

Alternativamente, muitas linguagens de programação oferecem bibliotecas que simplificam a leitura desses argumentos. Pode-se optar por essas alternativas dependendo da complexidade do input requerido.

Falando sobre implementação em Swift, a classe `CommandLine` fornece acesso aos argumentos de linha de comando. O primeiro argumento `CommandLine.arguments[0]` é sempre o nome do programa.

## Veja também:

Para mais detalhes, consulte:
- [Documentação da Apple sobre a classe CommandLine](https://developer.apple.com/documentation/swift/commandline)
- [Tutorial de Argumentos de Linha de Comando em Swift](https://www.raywenderlich.com/411-command-line-programs-on-macos-tutorial)
- [Post no StackOverflow sobre Argumentos de Linha de Comando em Swift](https://stackoverflow.com/questions/24029633/how-do-you-use-command-line-arguments-in-swift)