---
title:    "Kotlin: Convirtiendo una cadena a minúsculas"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué convertir una cadena a minúsculas?

A veces, en la programación, necesitamos tratar con cadenas en diferentes formatos. Una tarea común es convertir una cadena a minúsculas, lo que puede ser útil para fines de comparación o visualización. En Kotlin, hay una función integrada para hacer esta conversión de forma rápida y sencilla. En esta entrada del blog, te explicaré cómo usar esta función y también profundizaré en cómo funciona detrás de escena.

## Cómo hacerlo en Kotlin

En Kotlin, podemos usar la función "toLowerCase()" para convertir una cadena a minúsculas. Aquí hay un ejemplo de cómo usarlo:

```Kotlin
val cadenaMayusculas = "ESTA ES UNA CADENA EN MAYÚSCULAS"
val cadenaMinusculas = cadenaMayusculas.toLowerCase()
println(cadenaMinusculas)
```
El resultado de este código será "esta es una cadena en mayúsculas". Ahora, si queremos hacer el cambio en la misma cadena original sin crear una nueva variable, podemos usar la función "toLowerCase()" directamente en la cadena:

```Kotlin
var cadena = "CONVERTIR ESTA CADENA"
cadena = cadena.toLowerCase()
println(cadena)
```
El resultado de este código será "convertir esta cadena".

## Inmersión profunda

Detrás de escena, la función "toLowerCase()" utiliza el estándar de codificación Unicode para hacer la conversión de minúsculas. Reconoce los caracteres en mayúsculas y los sustituye por sus equivalentes en minúsculas.

Además, es posible que hayas notado que en los ejemplos anteriores usamos la palabra reservada "val" en lugar de "var" para declarar nuestras variables. Esto se debe a que en Kotlin, las cadenas se tratan como objetos inmutables, lo que significa que no podemos cambiar su valor después de su inicialización.

## Ver también

- Documentación oficial de Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Guía de estilo de strings en Kotlin: https://kotlinlang.org/docs/reference/coding-conventions.html#strings
- Tutoriales de Kotlin en español: https://www.danielprimo.io/tag/kotlin/