---
title:                "Gerando números aleatórios"
html_title:           "Kotlin: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Porque

Gerar números aleatórios é uma tarefa fundamental em muitas aplicações e pode fornecer resultados imprevisíveis e variáveis, tornando o processo mais interessante e dinâmico.

## Como Fazer

```kotlin
// Gerando um número inteiro aleatório entre 0 e 10
val randomInt = (0..10).random()

// Gerando um número decimal aleatório entre 0 e 1
val randomDouble = Math.random()

// Gerando uma lista com 5 números aleatórios entre 1 e 100
val randomList = (1..100).shuffled().take(5)
```

### Saída de Exemplo:

Número inteiro aleatório: 7

Número decimal aleatório: 0,522313

Lista de números aleatórios: [42, 87, 15, 99, 31]

## Deep Dive

A geração de números aleatórios é comumente usada em jogos, sorteios, criptografia e muitos outros casos. No entando, é importante entender que esses números não são verdadeiramente aleatórios, mas sim pseudorandomizados. Isso significa que são gerados através de um algoritmo matemático que usa um número inicial chamado de "seed" (semente) para gerar uma sequência de números que parecem aleatórios, mas na verdade são previsíveis e repetíveis quando a mesma semente é utilizada.

Para controlar a semente utilizada nos números aleatórios, podemos utilizar a classe `Random` da biblioteca padrão do Kotlin. Além disso, é possível definir limites e escolher entre diferentes tipos de números, como inteiros, decimais e booleanos.

## Veja Também

- [Documentação Oficial do Kotlin sobre `Random`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Artigo da Baeldung sobre Geração de Números Aleatórios em Kotlin](https://www.baeldung.com/kotlin/random)