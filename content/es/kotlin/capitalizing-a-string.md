---
title:    "Kotlin: Capitalizar una cadena"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena de texto es una tarea común en la programación. Al convertir todas las letras de una palabra o frase a mayúsculas, facilita su lectura y evita errores en comparaciones de cadenas. En este artículo, aprenderemos cómo capitalizar una cadena en Kotlin y exploraremos un poco más sobre este concepto.

## Cómo hacerlo

Para capitalizar una cadena en Kotlin, podemos utilizar la función `capitalize()`. Veamos un ejemplo:

```Kotlin
val cadena = "hola mundo"
println(cadena.capitalize())
```

Este código imprimirá "Hola mundo" en la consola. La función `capitalize()` convierte la primera letra de la cadena en mayúscula y el resto se mantiene igual.

También es posible capitalizar cada palabra de una cadena con la función `titlecase()`. Veamos cómo se ve con un ejemplo:

```Kotlin
val cadena = "bienvenidos a mi blog"
println(cadena.titlecase())
```

Este código imprimirá "Bienvenidos A Mi Blog". Podemos notar que la función `titlecase()` convierte la primera letra de cada palabra en mayúscula.

## Profundizando

Ahora que sabemos cómo capitalizar cadenas en Kotlin, veamos un poco más sobre qué sucede detrás de escena. Aunque parezca simple, esta función en realidad usa un algoritmo para determinar cuál letra debe ser capitalizada.

Por ejemplo, el algoritmo revisa si la letra anterior es un espacio, guión o carácter especial. Si es así, entonces la letra actual será capitalizada.

También es importante destacar que la función `capitalize()` solo capitalizará la primera letra de la cadena. Si la primera letra ya es mayúscula, no se realizarán cambios.

## Ver también

- [Documentación de la función `capitalize()` en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Documentación de la función `titlecase()` en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/titlecase.html)
- [Diferencias entre `capitalize()` y `titlecase()`](https://www.tutorialspoint.com/capitalize-vs-titlecase-in-kotlin)

¡Espero que este artículo les haya sido útil! Ahora podemos capitalizar cadenas en Kotlin sin problemas. ¡Hasta la próxima!