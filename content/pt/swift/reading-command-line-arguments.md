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

## O que & Por quê?

Ler argumentos da linha de comando é uma tarefa comum na programação Swift. Isso significa que um programa pode receber informações diretamente do usuário ao ser executado, em vez de ter que ser pré-definido. Programadores fazem isso para tornar seus programas mais interativos e flexíveis.

## Como fazer:

Para ler argumentos da linha de comando em Swift, usamos a propriedade "arguments" do objeto "CommandLine". Aqui está um exemplo simples que imprime os argumentos fornecidos pelo usuário:

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

Se executarmos este programa com o comando `swift program.swift argumento1 argumento2`, ele irá imprimir `[program.swift, argumento1, argumento2]`.

## Profundando:

Ler argumentos da linha de comando não é algo novo na programação. Na verdade, é uma técnica bastante utilizada em muitas linguagens de programação. Uma alternativa para isso em Swift seria usar a função "readLine" para receber entradas do usuário durante a execução do programa. No entanto, isso limitaria a flexibilidade e interatividade do programa.

Internamente, o Swift usa a biblioteca "libc" para ler os argumentos fornecidos pela linha de comando. Portanto, você também pode acessar essa biblioteca diretamente para uma implementação mais precisa.

## Veja também:

- [A documentação oficial do Swift sobre a propriedade "arguments"](https://developer.apple.com/documentation/swift/commandline/1640901-arguments)
- [Uma explicação mais detalhada sobre como ler argumentos da linha de comando em Swift](https://swiftrocks.com/reading-command-line-arguments-swift.html)
- [A documentação oficial da biblioteca "libc"](https://developer.apple.com/documentation/swift/swift_standard_library/libc)