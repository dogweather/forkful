---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:35.110207-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Gerar números aleatórios é como lançar dados virtuais – você obtém um valor que não consegue prever. Programadores fazem isso para funções que precisam de um bocado de imprevisibilidade, como jogos ou simulações.

## How to:
Kotlin torna fácil brincar com a sorte. Vamos a alguns exemplos.

Gerar um número aleatório entre 0 e 100:

```kotlin
val randomNumber = (0..100).random()
println(randomNumber)
```

Quer um ponto flutuante? Sem problemas:

```kotlin
val randomDouble = Math.random()
println(randomDouble)
```

Se você precisa de mais controle, a biblioteca `java.util.Random` é sua amiga:

```kotlin
import java.util.Random

val random = Random()
val randomNumber = random.nextInt(100) // Até 100
println(randomNumber)
```

## Deep Dive
Números "aleatórios" em programação são na verdade pseudoaleatórios – são previsíveis se você conhece o algoritmo e a semente ("seed"). Kotlin usa `java.util.Random` por baixo dos panos, que é decente para a maioria das aplicações mas não use para criptografia!

Há alternativas. Precisa de algo mais forte? Dê uma olhada em `SecureRandom` para valores mais imprevisíveis. Kotlin também suporta o uso de geradores de números aleatórios diferentes com seu módulo `kotlin.random.Random`, permitindo plataformas específicas como JavaScript ou Native.

No passado, a escolha do algoritmo era crítica e, dependendo da semente e do algoritmo, você poderia ter um resultado menos aleatório do que o esperado. Hoje em dia, os algoritmos são bem sofisticados.

## See Also
- Kotlin Standard Library: [Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- Java Platform, Standard Edition: [Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- SecureRandom for cryptography: [SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
