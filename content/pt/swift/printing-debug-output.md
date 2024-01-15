---
title:                "Imprimindo saída de depuração"
html_title:           "Swift: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Escrever código em qualquer linguagem de programação pode ser uma tarefa complicada e, muitas vezes, é necessário depurar o código para encontrar e corrigir erros. A impressão de saída de depuração é uma ferramenta valiosa para ajudar a entender o comportamento do código e encontrar problemas.

## Como Fazer

Para imprimir saída de depuração no Swift, é necessário utilizar a função `print()`. É importante notar que, por padrão, a saída de depuração é impressa no console do Xcode.

```Swift
let nome = "Maria"

// Impressão de saída de depuração
print("Olá, o meu nome é \(nome)")
```

A saída para esta linha de código seria `Olá, o meu nome é Maria` no console do Xcode.

Se você quiser imprimir saída de depuração em um arquivo ou no console do sistema, é possível usar o parâmetro `terminator` da função `print()`.

```Swift
let apelido = "Marry"

// Impressão de saída de depuração em um arquivo
print("O meu apelido é \(apelido)", terminator: "\n", to: &FileHandle.standardError)

// Impressão de saída de depuração no console do sistema
print("O meu apelido é \(apelido)", terminator: "\n", to: &FileHandle.standardOutput)
```

A saída para estas linhas de código seria `O meu apelido é Marry` em um arquivo ou no console do sistema.

Outra opção útil é o uso do símbolo de escape `%@` para imprimir objetos em formato de string.

```Swift
let idade = 28

// Impressão de saída de depuração
print("Eu tenho \(idade) anos")

// Impressão de saída de depuração usando o símbolo de escape
print("Eu tenho \(idade) anos", "ano(s)", separator: " ", terminator: "\n", to: &FileHandle.standardOutput)
```

A saída para esta linha de código seria `Eu tenho 28 anos ano(s)` no console do Xcode.

## Mergulho Profundo

É possível personalizar ainda mais a saída de depuração adicionando informações adicionais, como a data e hora em que a saída foi impressa, o nome do arquivo e o número da linha. Isso pode ser útil para rastrear a origem de um determinado problema no código.

```Swift
let data = Date()

// Impressão de saída de depuração com informações adicionais
print("A data e hora atual são: \(data)", "localizada no arquivo: \(#file), na linha: \(#line)", to: &FileHandle.standardError)
```

A saída para esta linha de código seria `A data e hora atual são: 2020-10-02 17:27:33 +0000 localizada no arquivo: Debugging.swift, na linha: 14` no console do Xcode.

## Veja também

- [The Basics of Printing in Swift](https://www.iosapptemplates.com/blog/swift/printing-swift)
- [Debugging in Xcode 11](https://medium.com/flawless-app-stories/debugging-in-xcode-11-2643dd7c76c9)
- [Debugging with Print Statements in Swift](https://medium.com/@sindresorhus/debugging-with-print-statements-in-swift-32d4965f20f2)