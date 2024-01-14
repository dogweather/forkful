---
title:                "Swift: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão, ou standard error, pode ser útil para diagnosticar e corrigir erros em um programa Swift. Ao escrever mensagens de erro específicas para o erro padrão, podemos identificar problemas e realizar depurações mais precisas.

## Como fazer?

Podemos escrever para o erro padrão utilizando a função `print(_:to:)` e especificando `stderr` no parâmetro `to`. Veja um exemplo abaixo:

```Swift
print("Erro: Valor inválido.", to: &stderr)
```

O código acima irá imprimir a mensagem de erro no console, no entanto, ela será destacada em vermelho para indicar que é uma mensagem de erro. O console pode ser encontrado na área de depuração do Xcode.

## Mergulho profundo

O erro padrão, ou standard error, é uma corrente de saída que é usada para imprimir mensagens de erro em vez de dados regulares. Ele é diferente do erro de saída padrão, o standard output, que é usado para a exibição normal de dados. Ao usar o `print(_:to:)` para escrever no erro padrão, estamos garantindo que a mensagem de erro será exibida de forma distinta das mensagens regulares no console.

## Veja também

- [Documentação oficial do Swift sobre a função `print(_:to:)`](https://developer.apple.com/documentation/swift/1541053-print)
- [Artigo sobre o uso do erro padrão em depurações de código em Swift](https://www.hackingwithswift.com/example-code/language/how-to-send-errors-to-the-debugger-using-custom-strings)
- [Tutorial sobre como lidar com erros em Swift](https://www.swiftbysundell.com/articles/handling-errors-in-swift/)