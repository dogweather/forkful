---
title:    "Kotlin: Extrayendo subcadenas"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en Kotlin?

Extraer subcadenas es una herramienta útil para aquellos que trabajan con cadenas de texto en Kotlin. Esto les permite obtener una parte específica de una cadena más grande, lo que puede ser útil para el procesamiento de datos y la manipulación de texto. Además, al dominar el concepto de extracción de subcadenas, se puede mejorar la eficiencia y la precisión del código.

## Cómo extraer subcadenas en Kotlin

La extracción de subcadenas en Kotlin se realiza utilizando el método `substring()` en una cadena de texto existente. Este método requiere dos parámetros: el índice de inicio y el índice de fin de la subcadena que desea extraer.

Por ejemplo, para extraer la subcadena "mundo" de la cadena "Hola mundo", se puede utilizar el siguiente código:

```Kotlin
val cadena = "Hola mundo"
val subcadena = cadena.substring(5,10)
println(subcadena) // Salida: mundo
```

También es posible especificar solo el índice de inicio, lo que extraerá la subcadena desde ese punto hasta el final de la cadena. Por ejemplo:

```Kotlin
val cadena = "Hola mundo"
val subcadena = cadena.substring(2)
println(subcadena) // Salida: la mundo
```

También se pueden utilizar índices negativos para referirse a la posición de la cadena desde el final en lugar del principio. Por ejemplo, para extraer la subcadena "mundo" de la cadena "Hola mundo", se podría usar el siguiente código:

```Kotlin
val cadena = "Hola mundo"
val subcadena = cadena.substring(5,-1)
println(subcadena) // Salida: mundo
```

También hay métodos alternativos disponibles para extraer subcadenas, como `subSequence()` y `slice()`, que ofrecen más opciones y flexibilidad.

## Profundizando en la extracción de subcadenas

La extracción de subcadenas en Kotlin es más que simplemente utilizar algunos métodos de cadena. También es importante entender cómo funcionan los índices en las cadenas y cómo manejar los casos de error, como los índices fuera de rango.

Además, al dominar esta técnica, puede aprovechar al máximo sus habilidades de codificación para manipular y transformar cadenas de manera eficiente y precisa.

## Ver también

- [Documentación oficial de Kotlin sobre la extracción de subcadenas](https://kotlinlang.org/docs/reference/basic-types.html#substring)
- [Tutorial de extracción de subcadenas en Kotlin](https://www.geeksforgeeks.org/extract-substring-kotlin/)
- [Vídeo tutorial de extracción de subcadenas en Kotlin](https://www.youtube.com/watch?v=h6-S9BUqaVE&t=125s)