---
title:                "Kotlin: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

A geração de números aleatórios é um recurso essencial em muitos programas de computador. Os números gerados de forma aleatória permitem simular situações do mundo real, criar jogos e testar algoritmos. Eles também podem ser úteis para criar senhas seguras ou para embaralhar listas de dados.

## Como fazer

Para gerar números aleatórios no Kotlin, podemos utilizar a função ```random()``` do pacote ```kotlin.math```. Veja abaixo um exemplo de como utilizar essa função:

```Kotlin
import kotlin.math.random

fun main() {
    // Gerando um número aleatório entre 0 e 10
    val numero = random.nextInt(10) 

    // Imprimindo o número gerado
    println(numero)
}
```

O código acima irá imprimir um número inteiro aleatório entre 0 e 10. Podemos utilizar a função ```nextInt()``` para especificar um limite máximo para o número gerado. Se não passarmos nenhum parâmetro, o limite padrão será 0.

## Aprofundando-se

Ao gerar números aleatórios, é importante entender como a linguagem de programação escolhida os gera. No Kotlin, a função ```random()``` é baseada no gerador de números pseudo-aleatórios do Java, que utiliza um algoritmo chamado *Linear Congruential Generator* (LCG).

Este algoritmo usa uma operação matemática simples para gerar uma sequência de números que aparentam ser aleatórios. No entanto, eles são determinísticos e, portanto, essa sequência pode ser repetida se o mesmo *seed* (semente) for usado.

Para evitar a repetição da sequência, podemos especificar manualmente um *seed* na função ```random()``` ou utilizar o valor padrão, que é baseado no tempo atual em milissegundos. Isso garante que a sequência seja diferente a cada execução do programa.

## Veja também

- [Documentação oficial do Kotlin sobre a função random()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.math/random.html)
- [Explicação detalhada sobre o funcionamento do LCG](https://www.geeksforgeeks.org/linear-congruential-generator-lcg/)
- [Tutorial sobre como gerar números aleatórios em Kotlin](https://www.baeldung.com/kotlin-random)