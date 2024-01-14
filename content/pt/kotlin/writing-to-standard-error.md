---
title:    "Kotlin: Escrevendo para o erro padrão"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão, também conhecido como stderr, pode ser útil para debugar seu código em Kotlin. Quando algum erro ocorre durante a execução do programa, a mensagem de erro é enviada para o erro padrão. Ao escrever diretamente para o erro padrão, você pode controlar melhor quais informações serão exibidas e como elas serão exibidas.

## Como escrever para o erro padrão em Kotlin

Para escrever para o erro padrão em Kotlin, você pode usar a função `System.err.println()`. Esta função recebe uma mensagem como parâmetro e a exibe no erro padrão. Por exemplo:

```Kotlin
fun main() {
    // Exemplo de mensagem de erro padrão
    System.err.println("Erro: Nome inválido.")
}
```

Neste exemplo, ao executar o programa, a mensagem "Erro: Nome inválido." será exibida no erro padrão.

## Deep Dive

Ao escrever para o erro padrão, é importante ter cuidado com o tipo de informação que está sendo exibida. Você não quer expor informações sensíveis ou críticas do seu código. Certifique-se de formatar corretamente a mensagem para que seja fácil de ler e entender. Além disso, você pode usar a função `System.exit()` para encerrar o programa em caso de erro, em vez de apenas imprimir a mensagem de erro no erro padrão.

## Veja também

- [Documentação oficial do Kotlin sobre a função System.err](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/system.err.html)

- [Tutorial sobre tratamento de exceções em Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_exceptions_handling.htm)

- [Artigo sobre a importância de escrever para o erro padrão](https://dzone.com/articles/why-print-to-stderr)