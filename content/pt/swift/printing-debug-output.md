---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Imprimir output de debug é a prática de saída de dados do programa para rastrear e entender o fluxo de execução. Programadores fazem isso para identificar falhas e resolver problemas mais rapidamente.

## Como fazer:

Aqui estão alguns exemplos de como imprimir na saída de depuração no Swift. Experimente-os em seu ambiente de codificação:

```Swift
// Imprimir texto simples
print("Olá, Mundo!")

// Imprimir valor de variável
let nome = "João"
print("Olá, \(nome)!")

// Imprimir com estruturas de controle
for i in 1...5 {
    print("Número: \(i)")
}

// Imprimir informações de debug
debugPrint("Esta é uma informação de debug")

// Imprimir usando o operador dump
let array = ["Maçã", "Banana", "Carambola"]
dump(array)
```

Isto vai produzir:

```Swift
Olá, Mundo!
Olá, João!
Número: 1
Número: 2
Número: 3
Número: 4
Número: 5
Esta é uma informação de debug
- "Maçã"
- "Banana"
- "Carambola"
```

## Mergulhando fundo

A prática de imprimir para depuração existe desde o início da linguagem de programação. No Swift, temos várias funções, como `print()`, `debugPrint()` e `dump()`, para imprimir saída de depuração. 

A função `print()` é a mais comum e usada para imprimir valores na saída padrão. A `debugPrint()`, por outro lado, é usada para imprimir valores de maneira adequada para depuração. 

Outra alternativa é o operador `dump()`. Este apresenta uma saída de depuração mais detalhada, útil quando você precisa de uma repartição mais completa do objeto.

## Veja também

Para aprofundar seu conhecimento sobre a depuração no Swift, confira esses recursos:

- Documentação oficial do Swift sobre a função `print()`: https://developer.apple.com/documentation/swift/1541053-print
- Guia detalhado da Ray Wenderlich sobre depuração no Swift: https://www.raywenderlich.com/113038/debugging-swift
- Dicas e truques de depuração do Swift no Medium: https://medium.com/@johnsundell/debugging-swift-code-d01717d1cfb5