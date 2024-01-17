---
title:                "Extrayendo subcadenas"
html_title:           "Kotlin: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¡Qué y por qué?

Extraer subcadenas es una técnica común en la programación en la que se toma una parte de una cadena más grande y se almacena como una cadena separada. Los programadores hacen esto para manipular y analizar datos más específicos en sus programas.

## ¡Cómo hacerlo!

``` kotlin
// Crear una cadena
val frase = "¡Hola a todos mis amigos!"

// Usando el método "substring", extraemos la subcadena "todos mis"
val subcadena = frase.substring(11, 19)

// Imprimimos la subcadena
println(subcadena)

// Output: todos mis
```

## Un poco más profundo

La extracción de subcadenas ha existido desde los primeros días de la programación de computadoras. Es una forma fácil y rápida de acceder a partes específicas de una cadena sin tener que manipularla directamente. Alternativamente, también se puede lograr utilizando la función "slice" en Kotlin. La implementación de la extracción de subcadenas en Kotlin es eficiente y optimizada para manejar grandes volúmenes de texto.

## Ver también

Enlaces a fuentes relacionadas:

- [Documentación oficial de la función "substring" en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Documentación oficial de la función "slice" en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/slice.html)