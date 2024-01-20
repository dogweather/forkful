---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Generar números aleatorios es crear una secuencia de números que no tiene patrones predecibles. Los programadores lo hacen para cosas como salpicar variedad en elementos de juego, o hacer criptografía segura.

## Cómo hacer:

Para generar números aleatorios en Kotlin, usamos la clase `Random`. Aquí tienes unos ejemplos:

```Kotlin
import kotlin.random.Random

fun main() {
    val randomInt = Random.nextInt(10)  // Genera un número entero aleatorio entre 0 (inclusive) y 10 (exclusivo)
    println(randomInt)  // Imprime el número generado
}
```

Este código genera e imprime un número aleatorio del 0 al 9.

Para un rango más personalizado, puedes hacer esto:

```Kotlin
import kotlin.random.Random

fun main() {
    val randomIntRange = Random.nextInt(50, 100)  // Genera un número aleatorio entre 50 (inclusive) y 100 (exclusivo)
    println(randomIntRange)  // Imprime el número generado
}
```

Esto imprimirá un número aleatorio entre 50 y 99.

## Análisis Profundo:

La generación de números aleatorios ha sido crucial en la informática desde sus inicios. Se hizo popular durante la segunda guerra mundial para cifrar comunicaciones. Kotlin hace uso de un tipo especial de generador de números pseudoaleatorios llamado Mersenne Twister, que es conocido por su alta calidad de aleatoriedad.

Aunque `Random` es la manera más común de generar números aleatorios en Kotlin, ten en cuenta que también existen otros métodos, como usando el método `Math.random()`, que puede ser conveniente si necesitas interoperabilidad con Java.

En cuanto a los detalles de implementación, recuerda que mientras `Random.nextInt()` genera un número entero aleatorio, `Random.nextDouble()` y `Random.nextFloat()` generan números con decimales. Todos estos métodos tienen sobrecargas para especificar rangos según necesites.

## Ver También:

Para una visión más profunda de la generación de números aleatorios en Kotlin, consulta los siguientes enlaces:

- Documentación oficial de Kotlin para la clase `Random`: [Random - Kotlin Programming Language](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- Un artículo útil sobre generadores de números aleatorios: [A Quick Look at Kotlin's Random Number Generators](https://www.baeldung.com/kotlin-random-number-generation)