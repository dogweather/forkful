---
title:                "Interpolando una cadena"
html_title:           "Kotlin: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Interpolar una cadena es una forma de insertar valores variables en una cadena de texto. Los programadores lo hacen porque les permite construir cadenas dinámicas con valores que pueden cambiar durante la ejecución del programa.

## Cómo:
```kotlin
val nombre = "Juan"
val edad = 25
val mensaje = "Hola, mi nombre es $nombre y tengo $edad años."
println(mensaje)
```
Output:
```
Hola, mi nombre es Juan y tengo 25 años.
```

## Deep Dive:
En el pasado, los programadores tenían que concatenar manualmente cadenas de texto y valores, lo que resultaba en código largo y confuso. Con la interpolación de cadenas, se simplifica el proceso de construir cadenas dinámicas en Kotlin. Otras alternativas a la interpolación de cadenas incluyen el uso de la función "format" o el uso de la clase "StringBuilder". En cuanto a la implementación, Kotlin utiliza la función especial "toString()" para convertir los valores a cadenas y luego los inserta en la cadena interpolada.

## Ver también:
- [Documentación oficial de interpolación de cadenas en Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation)
- [Artículo sobre interpolación de cadenas en Kotlin de Ray Wenderlich](https://www.raywenderlich.com/380-test-kotlin-text-input-kotlin-string-interpolation)