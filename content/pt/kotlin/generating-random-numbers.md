---
title:    "Kotlin: Gerando números aleatórios."
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante para a programação?

Muitas vezes, em programação, precisamos de criar números aleatórios para simular situações ou para tornar nosso programa mais dinâmico. Esses números podem ser usados para gerar valores em jogos, selecionar itens de uma lista ou até mesmo para testes de software. A geração de números aleatórios pode ser uma habilidade útil para ter em seu arsenal de programação.

## Como gerar números aleatórios em Kotlin?

```Kotlin
// Gerando um número aleatório entre 1 e 100
val randomNumber = (1..100).random()
println(randomNumber)
// Output: 62

// Gerando um número aleatório em uma lista
val options = listOf("maçã", "banana", "laranja", "morango")
val randomOption = options.random()
println(randomOption)
// Output: banana
```

## Mergulho profundo na geração de números aleatórios

A geração de números aleatórios não é realmente aleatória - é um processo chamado de pseudoaleatório. Isso significa que, usando um algoritmo matemático, podemos criar uma sequência de números que parecem aleatórios, mas são realmente determinísticos. O valor inicial usado para esse algoritmo é chamado de "semente" e é isso que garante que a mesma sequência de "números aleatórios" será gerada com a mesma semente.

Kotlin fornece diferentes maneiras de gerar números aleatórios, incluindo as funções `random()` e `nextInt()`. Você também pode especificar uma semente para controlar a sequência de números gerados.

## Veja também

- [Documentação oficial do Kotlin sobre geração de números aleatórios](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)
- [Tutorial do MindOrks sobre geração de números aleatórios em Kotlin](https://blog.mindorks.com/generating-random-numbers-in-kotlin)
- [Vídeo do canal Code With Aarnav sobre geração de números aleatórios em Kotlin](https://www.youtube.com/watch?v=Rx3P6vT6ZCc)