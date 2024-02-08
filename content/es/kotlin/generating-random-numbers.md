---
title:                "Generación de números aleatorios"
date:                  2024-01-27T20:34:46.727638-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios en programación trata de crear números que carezcan de cualquier patrón predecible. Los programadores hacen esto por diversas razones, incluyendo simulaciones, pruebas de algoritmos, juegos y aplicaciones de seguridad, donde la imprevisibilidad es clave para lograr resultados realistas o seguros.

## Cómo hacerlo:

Kotlin proporciona una manera sencilla de generar números aleatorios a través de su biblioteca estándar. Así es como puedes generar diferentes tipos de valores aleatorios:

### Generando un Entero Aleatorio

Para generar un entero aleatorio dentro de un rango específico:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Genera un número aleatorio entre 1 y 99
    println(randomNumber)
}
```

### Generando un Double Aleatorio

De manera similar, generando un double aleatorio:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Genera un double aleatorio entre 1.0 y 10.0
    println(randomDouble)
}
```

### Generando un Boolean Aleatorio

Para generar un valor boolean aleatorio:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Genera true o false al azar
    println(randomBoolean)
}
```

### Sembrando para Resultados Reproducibles

En casos donde necesitas secuencias reproducibles de números aleatorios (por ejemplo, en pruebas), puedes sembrar el generador de números aleatorios:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Inmersión Profunda

El enfoque de la biblioteca estándar de Kotlin para generar números aleatorios aprovecha el `java.util.Random` de Java por debajo del capó, asegurando una combinación de facilidad de uso y rendimiento. Sin embargo, es crucial tener en cuenta que estos métodos generan números pseudoaleatorios, lo que significa que los números parecen aleatorios pero se generan usando un proceso determinista.

Para la mayoría de las aplicaciones, la aleatoriedad proporcionada por la clase `Random` de Kotlin es suficiente. Sin embargo, para aplicaciones más sensibles a la seguridad, como la criptografía, donde la calidad de la aleatoriedad es de suma importancia, uno debería considerar usar `java.security.SecureRandom` en su lugar. SecureRandom está diseñado específicamente para operaciones criptográficas, proporcionando una calidad de aleatoriedad superior, aunque con un posible compromiso en rendimiento.

Kotlin no reinventa la rueda, pero ofrece una API amigable con Kotlin sobre los mecanismos de generación de números aleatorios de Java, haciéndolo más idiomático y conciso de usar dentro de proyectos Kotlin. Como siempre, al tratar con la aleatoriedad, los programadores deben considerar cuidadosamente el caso de uso para elegir la herramienta más apropiada para el trabajo.
