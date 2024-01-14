---
title:                "Kotlin: Extrayendo subcadenas"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué: Extractar Subcadenas en Kotlin

Muchos se preguntarán por qué es importante o necesario extraer subcadenas en Kotlin. La respuesta es simple: la manipulación de cadenas de texto es una tarea común en la programación y, en muchas ocasiones, necesitamos acceder a una parte específica de una cadena para realizar ciertas operaciones. La extracción de subcadenas nos permite hacer esto de manera sencilla y eficiente.

## Cómo hacerlo en Kotlin

Para extraer una subcadena en Kotlin, podemos utilizar el método `substring()` de la clase `String`. Este método toma dos parámetros: el índice inicial y el índice final de la subcadena que queremos extraer.

```Kotlin
val frase = "Hola Mundo!"
val subcadena = frase.substring(0, 4)

println(subcadena) // Output: "Hola"
```

En el ejemplo anterior, utilizamos el índice 0 como inicio y el índice 4 como final, lo que nos permitió extraer la subcadena "Hola" de la frase "Hola Mundo!".

También podemos utilizar `substring()` con un solo parámetro, que representa el índice inicial de la subcadena. En este caso, la subcadena se extraerá desde ese índice hasta el final de la cadena.

```Kotlin
val frase = "Hola Mundo!"
val subcadena = frase.substring(5)

println(subcadena) // Output: "Mundo!"
```

## Profundizando en la extracción de subcadenas

Además de los parámetros básicos, el método `substring()` también nos permite trabajar con índices negativos. Esto significa que podemos contar los índices desde el final de la cadena en lugar del principio.

```Kotlin
val frase = "Hola Mundo!"
val subcadena = frase.substring(0, -1)

println(subcadena) // Output: "Hola Mundo"
```

En este ejemplo, utilizamos -1 como índice final, lo que nos permite extraer la subcadena "Hola Mundo" sin incluir el último carácter "!".

También podemos utilizar `substring()` para extraer subcadenas de un tamaño específico a partir de un índice. Para ello, simplemente agregamos el número de caracteres que queremos extraer como tercer parámetro.

```Kotlin
val frase = "Hola Mundo!"
val subcadena = frase.substring(2, 6)

println(subcadena) // Output: "la M"
```

En este caso, hemos incluido solo los caracteres del índice 2 al 6, lo que nos ha dado como resultado la subcadena "la M".

## Ver también

- Documentación oficial de Kotlin sobre el método `substring()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html
- Tutorial de extracción de subcadenas en Kotlin: https://danilonovais.com.br/substring-kotlin/

¡Ahora estás listo para comenzar a extraer subcadenas en tus proyectos de Kotlin! Recuerda que esta es solo una de las muchas operaciones que puedes hacer con cadenas de texto en este lenguaje de programación. ¡Sigue explorando y descubrirás muchas más funcionalidades interesantes!