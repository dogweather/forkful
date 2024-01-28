---
title:                "Trabajando con números complejos"
date:                  2024-01-26T04:42:34.476111-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con números complejos"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Los números complejos amplían nuestro sistema numérico para incluir las raíces cuadradas de números negativos, donde la unidad "imaginaria" i es igual a la raíz cuadrada de -1. Los programadores los utilizan en campos como la ingeniería, la física y el procesamiento de señales, porque son excelentes para modelar olas, oscilaciones y cualquier cosa que gire.

## Cómo hacerlo:

Definamos una clase básica de número complejo en Kotlin:

```kotlin
data class Complex(val real: Double, val imaginario: Double) {
    operator fun plus(otro: Complex) = Complex(real + otro.real, imaginario + otro.imaginario)
    operator fun minus(otro: Complex) = Complex(real - otro.real, imaginario - otro.imaginario)
    operator fun times(otro: Complex) = Complex(
        real * otro.real - imaginario * otro.imaginario,
        real * otro.imaginario + imaginario * otro.real
    )
    
    override fun toString(): String = "($real + ${imaginario}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // Salida: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Salida: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Salida: a * b = (-5.0 + 10.0i)
}
```

## Estudio profundo

Los números complejos fueron mencionados por primera vez en el siglo 16, resolviendo ecuaciones cúbicas que carecían de soluciones reales. La ingeniería y la física se benefician enormemente de los números complejos para analizar circuitos de corriente alterna y formas de onda. Alternativamente, podrías utilizar una biblioteca como `koma` o `ejml` de Kotlin para trabajos de mayor envergadura.

Las operaciones sobre números complejos reflejan a los números reales, pero prestando atención a la unidad imaginaria. La multiplicación, por ejemplo, sigue la propiedad distributiva, recordando que `i^2 = -1`. Esta unidad imaginaria nos permite representar números multidimensionales, crucial en varias computaciones científicas.

## Ver también

Bibliotecas de matemáticas de Kotlin:

- [koma](https://koma.kyonifer.com/): Una biblioteca de cómputo científico para Kotlin.

Lectura adicional sobre Números Complejos:

- [Wikipedia: Números Complejos](https://es.wikipedia.org/wiki/Número_complejo)
