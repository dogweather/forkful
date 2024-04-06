---
date: 2024-01-20 17:51:34.250543-07:00
description: "C\xF3mo hacerlo: Salida."
lastmod: '2024-04-05T21:54:00.365464-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Interpolaci\xF3n de cadenas de texto"
weight: 8
---

## Cómo hacerlo:
```kotlin
fun main() {
    val nombre = "Miguel"
    val edad = 25
    val mensaje = "Hola, mi nombre es $nombre y tengo $edad años."
    println(mensaje)
}
```
Salida:
```
Hola, mi nombre es Miguel y tengo 25 años.
```

Para expresiones más complejas, usas llaves `{}`:
```kotlin
fun main() {
    val horasTrabajadas = 9
    val costoPorHora = 50
    val mensaje = "Hoy gané ${horasTrabajadas * costoPorHora} euros."
    println(mensaje)
}
```
Salida:
```
Hoy gané 450 euros.
```

## Inmersión profunda:
Antes de Kotlin, los programadores de Java utilizaban `String.format` o concatenación con `+`. La interpolación de cadenas en Kotlin, introducida desde su creación, simplifica el proceso al permitir la inserción directa de variables y expresiones dentro de una cadena de texto. Esto no solo limpia el código, sino que también es más eficiente en tiempo de ejecución comparado con la concatenación.

La interpolación se maneja mediante el uso del símbolo `$`, seguido directamente por el nombre de la variable o `{}` si se incluye una expresión. A nivel de implementación, el compilador de Kotlin traduce estas cadenas interpoladas a una construcción `StringBuilder`, lo cual es óptimo.

## Ver también:
- Documentación oficial de Kotlin sobre [cadenas de texto](https://kotlinlang.org/docs/basic-types.html#string-literals)
- Una explicación más exhaustiva de la eficiencia de [`StringBuilder`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
