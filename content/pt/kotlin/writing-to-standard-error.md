---
title:    "Kotlin: Escrevendo para o erro padrão"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Por que escrever para o erro padrão em Kotlin?

Escrever para o erro padrão, também conhecido como "escrever para a saída de erro", é um método útil para debugar e detectar erros em seu código Kotlin. Ao escrever para o erro padrão, você pode visualizar mensagens e dados no console que podem ajudá-lo a encontrar e corrigir problemas em seu código.

## Como fazer isso:

Escrever para o erro padrão em Kotlin é bastante simples. Tudo o que você precisa fazer é usar a função `System.err.println()`, seguida pelo conteúdo que deseja exibir no console. Veja um exemplo abaixo:

```
Kotlin System.err.println("Este é um exemplo de mensagem de erro.")
```

A saída desse código será uma mensagem de erro no console, indicando que algo precisa ser corrigido em sua aplicação. Dependendo do tipo de mensagem que deseja exibir, você também pode usar outras funções, como `System.err.print()` ou `System.err.printf()`.

## Aprofundando:

Escrever para o erro padrão é particularmente útil quando você está tentando encontrar e solucionar problemas em seu código. Ao exibir mensagens e dados no console, você consegue verificar se suas variáveis estão armazenando os valores corretos e se os cálculos e operações estão sendo executados da forma esperada.

Além disso, escrever para o erro padrão também pode ser útil em situações onde você precisa informar ao usuário sobre um erro específico que ocorreu. Você pode exibir uma mensagem de erro detalhada no console, indicando qual foi o problema e como ele pode ser resolvido.

## Veja também:

- [Manipulando exceções em Kotlin](https://kotlinlang.org/docs/reference/exceptions.html)
- [Tutorial sobre debugging em Kotlin](https://www.raywenderlich.com/1301773-debugging-kotlin-like-a-pro)
- [Discussão sobre técnicas de depuração em Kotlin](https://stackoverflow.com/questions/37670452/how-can-i-get-my-kotlin-code-to-use-the-debugger-properly)