---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:45.616408-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Generar números aleatorios significa crear valores que no puedes predecir. Los programadores los usan para juegos, simulaciones, pruebas de software y hasta en seguridad informática.

## Cómo Hacerlo:

En Kotlin, puedes generar números aleatorios fácilmente. Aquí tienes unos ejemplos:

```kotlin
import kotlin.random.Random

fun main() {
    val numeroAleatorio = Random.nextInt(0, 100) // Desde 0 (incluido) hasta 100 (excluido)
    println(numeroAleatorio)

    val numeroRealAleatorio = Random.nextDouble(1.0, 10.0) // Desde 1.0 hasta 10.0
    println(numeroRealAleatorio)
}
```

Salida de ejemplo:

```
42
5.9234
```

## Análisis Profundo:

Históricamente, generar números aleatorios que sean verdaderamente aleatorios es un desafío. Computadoras son deterministas, así que generar algo no determinista no es tan sencillo. La solución: algoritmos de números pseudoaleatorios (PRNG). Un PRNG utiliza semillas (valores iniciales) para producir secuencias que parecen aleatorias.

Alternativas a `Random` de Kotlin incluyen `ThreadLocalRandom` para multihilo o librerías de terceros como `java.security.SecureRandom` para necesidades criptográficas.

Detalle de implementación: Kotlin utiliza `java.util.Random` o una instancia de `SplittableRandom` dependiendo si estás en JVM o JavaScript respectivamente. Esto permite que Kotlin proporcione una experiencia consistente a través de plataformas.

## Ver También:

- Documentación oficial de Kotlin sobre aleatoriedad: [Random - Kotlin Programming Language](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- Post sobre aleatoriedad en la programación: [Random number generation - Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation)
