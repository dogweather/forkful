---
title:    "Kotlin: Gerando números aleatórios"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante na programação?

Gerar números aleatórios é uma habilidade fundamental para qualquer programador, independentemente da linguagem de programação que esteja utilizando. Ele permite que você crie um elemento de aleatoriedade em seus programas, tornando-os mais dinâmicos e imprevisíveis. Essa imprevisibilidade é útil em jogos, sorteios, algoritmos de inteligência artificial e muitos outros casos de uso.

## Como gerar números aleatórios em Kotlin

Para gerar números aleatórios em Kotlin, podemos usar a classe `Random` da biblioteca padrão da linguagem. Esta classe possui vários métodos úteis para gerar números inteiros e decimais aleatórios. Veja abaixo alguns exemplos de como usar esta classe:

```Kotlin
// Importando a classe Random
import kotlin.random.Random

// Gerando um número inteiro aleatório entre 0 e 10
val randomNumber = Random.nextInt(11)

// Gerando um número decimal aleatório entre 0.0 e 1.0
val randomDecimal = Random.nextDouble()

// Gerando um número inteiro aleatório entre 20 e 30
val randomRange = Random.nextInt(20, 31)

// Gerando um número booleano aleatório
val randomBoolean = Random.nextBoolean()
```

Aqui vemos que podemos especificar o intervalo de números nos métodos `nextInt()` e `nextDouble()`, e também podemos gerar um número booleano aleatório com o método `nextBoolean()`.

## Aprofundando no assunto

A criação de números aleatórios pode ser algo complexo e desafiador, especialmente quando se trata de simular situações reais em que a aleatoriedade é uma parte importante. Existem várias técnicas e algoritmos avançados para gerar números verdadeiramente aleatórios, mas na maioria dos casos, o uso da classe `Random` da biblioteca padrão é suficiente.

É importante lembrar que os números gerados pela classe `Random` são pseudoaleatórios, ou seja, eles são gerados por um algoritmo e, portanto, podem ser reproduzidos se o mesmo algoritmo for usado. Se você precisa de maior aleatoriedade em seus programas, é recomendado usar uma biblioteca externa que implemente técnicas mais avançadas de geração de números aleatórios.

## Veja também

- [Documentação oficial da classe Random em Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Artigo sobre números aleatórios em programação](https://medium.com/@JorgeCastilloPr/tips-to-build-a-rock-solid-game-intuition-around-randomness-2227d9b7461)
- [Biblioteca externa de geração de números aleatórios em Kotlin](https://github.com/dslomov/kotlin-random)
- [Vídeo sobre números aleatórios em Kotlin](https://www.youtube.com/watch?v=DrQWVwB5wgI)