---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Gerar números aleatórios é o ato de produzir valores sem um padrão previsível. Os programadores fazem isso para tarefas que exigem sortear um resultado, como jogos, testes de software e simulações.

## Como Fazer:

No Kotlin, a geração de números aleatórios é feita de forma bastante simples e direta. Veja como você pode gerar um número aleatório:

```Kotlin
import kotlin.random.Random

fun main() {
    val numeroAleatorio = Random.nextInt(1, 100) // gera um número aleatório entre 1 e 100
    println(numeroAleatorio)
}
```

Quando você executa este código, a saída será algum número entre 1 e 100.

## Mergulho Profundo

Os números aleatórios têm uma longa história na computação, desde o sorteio de números para jogos até a criação de chave de segurança em criptografia.

Historicamente, a geração de números aleatórios era realizada externamente e carregada na máquina. Hoje, os números aleatórios são gerados através de uma série de algoritmos.

As alternativas ao Kotlin para gerar números aleatórios estão em quase todas as linguagens de programação, incluindo Java e Python. No entanto, o Kotlin simplifica muito a tarefa.

A implementação do Kotlin para números aleatórios usa `java.util.Random` sob o capô, mas o Kotlin torna a API mais fácil de usar e mais segura, evitando alguns dos erros comuns encontrados ao usar a classe Random do Java diretamente.

## Veja Também:

1. Documentação oficial do Kotlin sobre random: [Aqui](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
2. Random no Python: [Aqui](https://docs.python.org/3/library/random.html)
3. Random no Java: [Aqui](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)