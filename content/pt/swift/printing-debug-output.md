---
title:    "Swift: Imprimindo saída de depuração"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

Então você está construindo um aplicativo incrível com Swift, mas de repente ele começa a apresentar bugs indesejados. Como encontrar a causa desses bugs? Bem, é aí que entra a saída de depuração. Imprimir mensagens do código pode fornecer informações valiosas sobre o que está acontecendo nos bastidores e ajudar a encontrar e corrigir erros.

## Como fazer

Existem algumas maneiras de imprimir saída de depuração no Swift. A maneira mais comum é usar a função `print()`, que recebe um ou mais parâmetros e imprime o conteúdo na janela de console. Veja um exemplo abaixo:

```Swift
let num1 = 5
let num2 = 10
print("A soma de \(num1) e \(num2) é \(num1 + num2)")
```

Este código imprimirá a seguinte saída no console:

`A soma de 5 e 10 é 15`

Além disso, o Xcode possui uma ferramenta de depuração integrada que permite visualizar variáveis, valores e até mesmo rastros de pilha enquanto o código está sendo executado. Isso pode ser muito útil para encontrar problemas em tempo real.

## Mergulho aprofundado

A opção de personalizar a função `print()` é o que torna a saída de depuração tão poderosa. Você pode imprimir valores de diferentes tipos de dados, formatar strings e também criar suas próprias funções de depuração com base nas necessidades do seu projeto. Além disso, o uso do log de depuração é uma boa prática para ajudar a manter um código limpo e organizado.

## Veja também

- [Documentação do Xcode: Debugging with Xcode](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/debugging_tools.html)
- [Dicas para saída de depuração no Swift](https://medium.com/@morio/debugging-tips-in-swift-705a27d9ce86)
- [Blog da Swift: Introdução à saída de depuração](https://www.swiftbysundell.com/posts/introduction-to-debugging-in-swift)