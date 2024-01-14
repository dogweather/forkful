---
title:    "Kotlin: Generando números aleatorios"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Kotlin?

Generar números aleatorios es una técnica comúnmente utilizada en programación para simular situaciones de incertidumbre, crear juegos, realizar pruebas y muchas otras aplicaciones. En Kotlin, podemos utilizar algunas funciones y métodos para generar números aleatorios de manera sencilla y eficiente.

## Cómo generar números aleatorios en Kotlin

Para generar números aleatorios en Kotlin, primero debemos importar la clase `Random` de la biblioteca estándar. Luego, podemos llamar al método `nextInt()` para obtener un número entero aleatorio. Veamos un ejemplo:

```Kotlin
import java.util.Random

fun main() {
    val random = Random()
    val numero = random.nextInt(100)

    println(numero)
}
```

Este código imprimirá un número aleatorio entre 0 y 99 cada vez que se ejecute. También podemos especificar un rango de números dentro del método `nextInt()` para obtener un número aleatorio dentro de ese rango. Por ejemplo, `random.nextInt(10)` generará un número aleatorio entre 0 y 9.

Para generar un número decimal aleatorio, podemos utilizar el método `nextDouble()` de la clase `Random`. Este método generará un número decimal aleatorio entre 0.0 y 1.0. También podemos especificar un rango de números decimales utilizando `random.nextDouble(n)`, donde `n` es el número máximo que queremos incluir en el rango.

```Kotlin
import java.util.Random

fun main() {
    val random = Random()
    val decimal = random.nextDouble(10.0)

    println(decimal)
}
```

Este código generará un número decimal aleatorio entre 0.0 y 10.0 cada vez que se ejecute.

## Profundizando en la generación de números aleatorios en Kotlin

El proceso de generación de números aleatorios se basa en un algoritmo que utiliza una semilla para generar una secuencia de números. En Kotlin, la semilla se genera automáticamente cada vez que se crea una nueva instancia de la clase `Random`, por lo que no es necesario especificar una semilla manualmente.

Sin embargo, si queremos asegurarnos de que se generen las mismas secuencias de números aleatorios cada vez que se ejecute el programa, podemos especificar una semilla al crear la instancia de `Random`.

```Kotlin
import java.util.Random

fun main() {
    val random = Random(12345) // especificando una semilla
    val numero = random.nextInt(100)

    println(numero)
}
```

Otra forma de generar un número aleatorio en Kotlin es utilizando la función `random()` de la biblioteca estándar. Esta función devuelve un número aleatorio de punto flotante entre 0.0 y 1.0.

```Kotlin
fun main() {
    val numero = Math.random() // se puede utilizar sin importar la clase Math
    println(numero)
}
```

## Ver también

- [Documentación oficial de Kotlin sobre generación de números aleatorios](https://kotlinlang.org/docs/reference/basic-types.html#random-number-generation) 
- [Funciones matemáticas en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.math/) 
- [Generador de números aleatorios en línea](https://www.random.org/)