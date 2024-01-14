---
title:    "Kotlin: Buscando y reemplazando texto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué buscar y reemplazar texto en Kotlin?

Buscar y reemplazar texto es una tarea común en todos los lenguajes de programación, y Kotlin no es una excepción. Esta técnica es útil para corregir errores, actualizar variables y realizar otras modificaciones en nuestro código de manera rápida y eficiente.

## Cómo hacerlo en Kotlin

Para realizar una búsqueda y reemplazo de texto en Kotlin, podemos utilizar la función `replace()` que está disponible en la clase `String`. Esta función recibe dos parámetros: el texto que queremos reemplazar y el texto por el que lo queremos sustituir. A continuación, un ejemplo y su respectivo resultado:

```Kotlin
val texto = "¡Hola Mundo!"
val nuevoTexto = texto.replace("Hola", "Hello")

println(nuevoTexto) // Resultado: ¡Hello Mundo!
```

En este ejemplo, utilizamos la función `replace()` para cambiar la palabra "Hola" por "Hello" en la variable `texto`.

## Un poco más profundo

La función `replace()` también acepta expresiones regulares como parámetros. Esto significa que podemos realizar búsquedas y reemplazos más complejos. Por ejemplo:

```Kotlin
val texto = "123456789"
val nuevoTexto = texto.replace("[0-5]".toRegex(), "X")

println(nuevoTexto) // Resultado: XXXXX6789
```

En este caso, utilizamos una expresión regular para reemplazar todos los números del 0 al 5 por la letra "X" en la variable `texto`.

Otra técnica que podemos utilizar es la función `replaceFirst()` que, como su nombre indica, reemplaza solo la primera ocurrencia del texto que estemos buscando. Un ejemplo práctico podría ser:

```Kotlin
val texto = "Bienvenido a mi blog, bienvenido a mi vida"
val nuevoTexto = texto.replaceFirst("bienvenido", "Welcome")

println(nuevoTexto) // Resultado: Welcome a mi blog, bienvenido a mi vida
```

## Ver también

- [Documentación oficial de la función replace en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Expresiones regulares en Kotlin](https://blog.kotlin-academy.com/regular-expressions-in-kotlin-60aa0a8b94ff)