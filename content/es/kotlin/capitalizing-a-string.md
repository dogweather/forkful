---
title:                "Capitalizando una cadena de texto"
html_title:           "Kotlin: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúsculas. Los programadores lo hacen para mejorar la legibilidad y la presentación visual del texto en las interfaces de usuario.

## Cómo hacerlo:
```Kotlin
val cadena = "hola, mundo kotlin"
val cadenaCapitalizada = cadena.capitalize()
println(cadenaCapitalizada)
```
Salida:
```
Hola, mundo kotlin
```
En la cadena de ejemplos, `"hola, mundo kotlin"` se convirtió en `"Hola, mundo kotlin"` después de usar el método `capitalize()`.

## Análisis Profundo
Kotlin hereda muchas de sus funciones de manejo de cadenas de Java, incluida la función para capitalizar una cadena. Como alternativa, puedes usar la función `toLowerCase()`, que convierte todas las letras de la cadena a minúsculas, y luego convertir solo la primera letra a mayúsculas. Cabe destacar que solo se capitaliza la primera letra del primer valor de la cadena, y no cada palabra individual.

```Kotlin
val cadena = "hola, mundo kotlin"
val primeraLetra = cadena[0].uppercase()
val restoDeCadena = cadena.substring(1)
val cadenaCapitalizada = primeraLetra + restoDeCadena
println(cadenaCapitalizada)
```
Salida:
```
Hola, mundo kotlin
```
Mencionar también que la función `uppercase()` está disponible a partir de Kotlin 1.5, reemplazando la función `toUpperCase()`.

## Ver También
Consulta la documentación oficial de Kotlin para más detalles:
1. [capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
2. [toUpperCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
3. [toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
4. [uppercase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/uppercase.html)