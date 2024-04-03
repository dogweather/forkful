---
date: 2024-01-20 17:53:02.557226-07:00
description: "Como Fazer: Em Kotlin, voc\xEA pode utilizar `println()` para mostrar\
  \ mensagens de debug."
lastmod: '2024-03-13T22:44:46.544143-06:00'
model: gpt-4-1106-preview
summary: "Em Kotlin, voc\xEA pode utilizar `println()` para mostrar mensagens de debug."
title: "Exibindo sa\xEDdas de depura\xE7\xE3o"
weight: 33
---

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
