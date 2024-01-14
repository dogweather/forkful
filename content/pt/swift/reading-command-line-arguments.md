---
title:                "Swift: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao criar um programa, precisamos que ele seja executado com diferentes valores de entrada. É aí que entram os argumentos da linha de comando. Eles permitem que os usuários informem valores específicos ao executar o programa, tornando-o mais versátil e personalizável.

## Como Fazer

Para ler argumentos da linha de comando em um programa Swift, podemos usar o objeto `CommandLine`. Veja um exemplo abaixo:

```
// Código Swift para ler argumentos da linha de comando

let args = CommandLine.arguments
print("O número de argumentos é \(args.count)")
print("Os argumentos são: \(args)")
```

Com `CommandLine.arguments`, podemos acessar uma matriz que contém todos os argumentos introduzidos pelo usuário. No segundo comando `print`, estamos imprimindo o número total de argumentos e, em seguida, imprimindo todos eles em uma única string.

Agora, podemos passar alguns argumentos para nosso programa quando o executamos no terminal. Por exemplo:
```
➜ swift MyProgram.swift Hello world
O número de argumentos é 3
Os argumentos são: ["Hello", "world"]
```

Neste caso, `"MyProgram.swift"` é o nome do nosso programa, `"Hello"` é o primeiro argumento e `"world"` é o segundo argumento. Podemos usar esses valores dentro do nosso código para executar diferentes ações com base nas entradas do usuário.

## Deep Dive

Além de acessar a matriz de argumentos inteira com `CommandLine.arguments`, podemos acessar argumentos individuais pelo seu índice. Por exemplo, se quisermos acessar apenas o segundo argumento, podemos usar `CommandLine.arguments[1]`, já que os índices começam em 0.

Também podemos usar `CommandLine.arguments.dropFirst()` para ignorar o primeiro argumento, que geralmente é o nome do nosso programa. Isso pode ser útil quando só queremos trabalhar com os argumentos fornecidos pelo usuário.

## Veja Também

- [Documentação Oficial da Apple sobre `CommandLine`](https://developer.apple.com/documentation/foundation/commandline)
- [Tutorial em português sobre argumentos da linha de comando em Swift](https://medium.com/@bcm/argumentos-da-linha-de-comando-em-swift-6f79b9ee8ae2)
- [Como passar argumentos da linha de comando para um programa Swift no terminal](https://www.hackingwithswift.com/example-code/system/how-to-get-command-line-arguments-using-swift)