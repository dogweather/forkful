---
title:                "Kotlin: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no standard error?

Existem várias razões pelas quais você pode querer escrever para o standard error em seus programas Kotlin.

Uma delas é que, ao contrário do standard output, que é destinado a exibir informações ao usuário, o standard error é usado para exibir mensagens de erro ou exceções em seu código. Isso pode ser útil para depuração e identificação de problemas em seu programa.

Além disso, algumas ferramentas e sistemas, como o JUnit, consideram a saída do standard error ao avaliar a execução de testes. Portanto, escrever para o standard error pode ajudar a identificar e resolver erros em seus testes.

## Como fazer

Para escrever no standard error em Kotlin, você pode usar a função `System.err.println()`, que imprimirá sua mensagem no standard error. Veja um exemplo abaixo:

```kotlin
fun main() {
    val num1 = 10
    val num2 = 0
    try {
        val result = num1 / num2
    } catch (e: ArithmeticException) {
        System.err.println("Não é possível dividir por zero!")
    }
}
```

A saída desse código será "Não é possível dividir por zero!" impresso no standard error.

## Mergulho Profundo

Ao escrever para o standard error, é importante lembrar que ele é apenas um fluxo de saída e não um local de armazenamento. Isso significa que as mensagens escritas no standard error a cada momento serão imediatamente exibidas e descartadas.

Além disso, é importante decidir com cuidado o que escrever para o standard error, pois muitas mensagens podem dificultar a leitura da saída do seu programa e dificultar a identificação de problemas reais.

## Veja também

- [Documentação oficial do Kotlin sobre a função System.err.println()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-print-stream/println.html)
- [Guia de Exceções e Erros em Kotlin](https://kotlinlang.org/docs/reference/exceptions.html)
- [Documentação oficial do JUnit sobre o uso do standard error](https://junit.org/junit5/docs/current/user-guide/#writing-tests-assertions)