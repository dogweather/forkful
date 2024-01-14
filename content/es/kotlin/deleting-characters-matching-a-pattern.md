---
title:    "Kotlin: Eliminando personajes que coinciden con un patrón."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón puede ser una tarea útil y necesaria en la programación de Kotlin. Esto puede ahorrar tiempo y mejorar la eficiencia al trabajar con grandes cantidades de texto o datos.

## Cómo

Para eliminar caracteres con un patrón específico en Kotlin, podemos utilizar la función `replace` en la cadena de texto a la que queremos aplicar el cambio. Por ejemplo, si queremos eliminar todas las vocales de una palabra, podríamos escribir lo siguiente en nuestro código:

```Kotlin
val palabra = "Hola"
val nuevaPalabra = palabra.replace("[aeiou]".toRegex(), "")
println(nuevaPalabra) // imprime "Hl"
```

Este código utiliza una expresión regular para encontrar y eliminar cualquier vocal de la palabra original. Podemos utilizar diferentes patrones y expresiones regulares para adaptar la función a nuestras necesidades específicas.

## Deep Dive

La función `replace` también nos permite reemplazar los caracteres que coinciden con nuestro patrón con otro texto. Además, podemos utilizar el parámetro `ignoreCase` para que la búsqueda de caracteres sea insensible a mayúsculas y minúsculas.

Además, es importante tener en cuenta que la función `replace` devuelve una nueva cadena de texto con los cambios realizados, pero no modifica la cadena original. Esto es útil para mantener la integridad de nuestros datos originales.

## Ver También

- [Documentación oficial de Kotlin sobre la función replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html)
- [Ejemplos de expresiones regulares en Kotlin](https://android.jlelse.eu/a-guide-to-regular-expressions-in-kotlin-78e753a1302b)
- [Tutorial interactivo sobre expresiones regulares en Kotlin](https://www.resocoder.com/2019/07/18/kotlin-regular-expressions-complete