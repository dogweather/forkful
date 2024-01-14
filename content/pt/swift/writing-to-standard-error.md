---
title:    "Swift: Escrevendo para o erro padrão"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Por que escrever para o erro padrão é importante?

Escrever para o erro padrão é uma habilidade essencial em programação Swift. Quando você sabe como direcionar seus erros para o lugar certo, pode economizar muito tempo na depuração de seu código e entender melhor como seu programa está se comportando. Neste post, vamos explorar por que a escrita para o erro padrão é tão importante e como fazer isso em Swift.

## Como fazer:

É muito simples escrever para o erro padrão em Swift. Você só precisa usar a função `print` e especificar `stderr` como o destino. Vamos ver um exemplo abaixo:

```swift
print("Erro encontrado!", to: &stderr)
```
O uso do `&` antes do `stderr` indica que estamos passando referência à `stderr` em vez de seu valor. Isso significa que a saída será gravada no fluxo de erro em vez do fluxo de saída normal.

## Profundidade:

Agora vamos mergulhar um pouco mais fundo e entender melhor por que escrever para o erro padrão é tão útil. Quando seu programa encontra um erro, geralmente é importante que você saiba exatamente onde ele ocorreu. Isso ajuda a entender melhor o que pode estar causando o erro e como resolvê-lo. Ao escrever para o erro padrão, você pode especificar uma mensagem mais descritiva e até mesmo imprimir valores de variáveis importantes para ajudar na depuração.

Assim como o fluxo de saída normal, o fluxo de erro também é impresso na tela do usuário. Isso significa que você pode usar a função `print` para informar ao usuário sobre erros em potencial que possam afetar a experiência deles com seu aplicativo.

# Veja também:

- [Documento oficial da Apple sobre a função `print`](https://developer.apple.com/documentation/swift/1541053-print)
- [Artigo sobre a importância da depuração em programação](https://medium.com/@camilaarguelhesoares/a-import%C3%A2ncia-da-depura%C3%A7%C3%A3o-em-programa%C3%A7%C3%A3o-1ef4797db17d)
- [Tutorial sobre como lidar com erros em Swift](https://www.hackingwithswift.com/new-syntax-swift-2-error-handling-try-catch)