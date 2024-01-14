---
title:                "Kotlin: Escrevendo para o erro padrão"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Por que escrever para o erro padrão é importante

Escrever para o erro padrão é uma prática essencial para programadores Kotlin, pois permite identificar e resolver problemas em seus códigos de forma mais eficiente. Além disso, é uma maneira de fornecer informações importantes sobre o funcionamento do programa ao usuário final.

Como fazer

Para escrever para o erro padrão em Kotlin, é necessário importar a classe `System` e utilizar o método `err`. Veja um exemplo abaixo:

```Kotlin
import java.lang.System

fun main(args: Array<String>) {
    System.err.println("Um erro ocorreu!")
}
```

Neste exemplo, estamos utilizando a função `println` para imprimir a mensagem de erro para o erro padrão. É importante notar que a utilização do `err` garante que a mensagem será exibida em vermelho, facilitando a sua identificação.

Profundidade de mergulho

É possível especificar o tipo de erro ao utilizar o método `err` no seu código. Por exemplo, se você quiser indicar um erro de bug, pode usar o código `System.err.println("Um erro de bug ocorreu!")`. Além disso, também é possível utilizar o `printStackTrace()` para imprimir o stack trace completo do erro, fornecendo informações mais detalhadas sobre sua ocorrência.

Ver também

- [Como escrever para o erro padrão em Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/system/i-o/err.html)
- [Documentação oficial do Kotlin](https://kotlinlang.org/docs/reference/)
- [Tutorial do Kotlin para iniciantes](https://www.devmedia.com.br/introducao-ao-kotlin/40247)