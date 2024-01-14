---
title:                "Kotlin: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer substrings es una técnica valiosa en programación que nos permite extraer secciones específicas de una cadena de texto. Esto puede ser útil en situaciones en las que solo necesitemos una parte de la información contenida en una cadena, como por ejemplo, un número de teléfono o una fecha.

## Cómo hacerlo

Para extraer substrings en Kotlin, podemos utilizar el método `substring()` en una cadena de texto. El método toma dos parámetros: el índice de inicio y el índice de fin de la sección que queremos extraer. Por ejemplo, si queremos obtener los primeros cinco caracteres de una cadena, podemos hacer lo siguiente:

```Kotlin
val texto = "¡Hola amigos!"
val subtexto = texto.substring(0, 5)
println(subtexto) // imprimirá "¡Hola"
```

También podemos utilizar índices negativos para empezar a contar desde el final de la cadena. Por ejemplo, si queremos obtener los últimos siete caracteres de una cadena, podemos hacer lo siguiente:

```Kotlin
val texto = "¡Hola amigos!"
val subtexto = texto.substring(texto.length - 7, texto.length)
println(subtexto) // imprimirá "amigos!"
```

## Profundizando

Al utilizar el método `substring()` en una cadena, debemos tener en cuenta que el índice de inicio es inclusivo, mientras que el índice de fin es exclusivo. Esto significa que el carácter en el índice de inicio será incluido en el substring, pero el carácter en el índice de fin no.

También podemos utilizar otros métodos relacionados como `substringBefore()` y `substringAfter()` para extraer substrings basados en un determinado carácter o cadena de texto.

## Ver también

- [Documentación oficial de Kotlin sobre extracción de substrings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Tutorial de extracción de substrings en Kotlin](https://www.petanikode.com/kotlin-substring/)
- [Ejemplos y explicaciones de extracción de substrings en Kotlin](https://www.programiz.com/kotlin-programming/substring)