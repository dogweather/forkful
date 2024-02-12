---
title:                "Exibindo saídas de depuração"
aliases:
- /pt/kotlin/printing-debug-output/
date:                  2024-01-20T17:53:02.557226-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Mostrar outputs de debug é basicamente imprimir mensagens no console do seu programa para entender o que está acontecendo durante a execução. Programadores fazem isso para rastrear bugs ou para ter certeza de que tudo está funcionando como deveria.

## Como Fazer:

Em Kotlin, você pode utilizar `println()` para mostrar mensagens de debug:

```kotlin
fun main() {
    val situacao = "debugando"
    println("Estou $situacao o Kotlin!")
    // Outra maneira é usar string template
    val resultado = 42
    println("O resultado é $resultado")
}

// Saída:
// Estou debugando o Kotlin!
// O resultado é 42
```

## Mergulho Profundo

O uso de outputs de debug é uma prática desde os primórdios da programação, surgindo como uma ferramenta simples, porém eficaz, para entender o comportamento dos programas. Em Kotlin, além do `println()`, você tem alternativas como o uso de loggers, que oferecem diferentes níveis de log (INFO, DEBUG, WARN, ERROR). Com loggers, é possível controlar melhor o que é mostrado, dependendo do ambiente de execução (desenvolvimento, teste, produção). Implementar um logger adequado é uma boa prática, especialmente em aplicações maiores, pois `println()` pode se tornar invasivo e difícil de gerenciar.

## Veja Também:

Para mais informações e aprofundamento nas práticas de debug e log em Kotlin, consulte os seguintes links:

- [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging) - Uma biblioteca de logging leve para Kotlin.
- [Documentação oficial do Kotlin](https://kotlinlang.org/docs/reference/) - Para entender mais sobre Kotlin e boas práticas de programação.
- [Logback](http://logback.qos.ch/) - Um framework de logging usado frequentemente com Kotlin para aplicações robustas.
