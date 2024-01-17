---
title:                "Eliminando caracteres coincidentes con un patrón"
html_title:           "Kotlin: Eliminando caracteres coincidentes con un patrón"
simple_title:         "Eliminando caracteres coincidentes con un patrón"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La eliminación de caracteres que coinciden con un patrón es una técnica común utilizada por los programadores para eliminar ciertos caracteres de una cadena de texto que coinciden con un patrón específico. Esto se utiliza a menudo para limpiar y formatear datos en una cadena. Por ejemplo, si queremos eliminar todos los números de una cadena, podemos usar la eliminación de caracteres que coinciden con el patrón "[0-9]". Esta técnica es útil para garantizar que los datos estén en el formato correcto y también para mejorar la legibilidad del código.

## Cómo:
```Kotlin
// Ejemplo 1: Eliminar todos los números de una cadena
val texto = "2a1b3c4d"
val nuevoTexto = texto.replace("[0-9]".toRegex(), "")
// nuevoTexto = "abcd"

// Ejemplo 2: Eliminar todas las vocales de una cadena
val texto = "Hola mundo"
val nuevoTexto = texto.replace("[aeiou]".toRegex(), "")
// nuevoTexto = "Hl mnd"

// Ejemplo 3: Eliminar todos los caracteres no alfanuméricos de una cadena
val texto = "¡Hola, mundo!"
val nuevoTexto = texto.replace("[^A-Za-z0-9 ]".toRegex(), "")
// nuevoTexto = "Hola mundo"
```

## Profundizando:
La técnica de eliminación de caracteres que coinciden con un patrón es muy común en la programación. Se deriva del concepto de expresiones regulares, que son patrones utilizados para buscar, validar y manipular texto. Además de usar el método `replace()` con un patrón de expresión regular, también se pueden usar otras funciones, como `replaceFirst()` y `removeRange()`, para eliminar caracteres según un patrón en Kotlin.

Aparte de la eliminación de caracteres con patrón, también existen otras técnicas para limpiar y formatear datos, como la tokenización y la manipulación de cadenas con métodos como `substring()`, `split()` y `trim()`. También es importante tener en cuenta otras consideraciones, como el rendimiento y la eficiencia, al realizar estas acciones en grandes cantidades de datos.

En cuanto a la implementación en Kotlin, la eliminación de caracteres con patrón se puede realizar de forma sencilla utilizando el método `replace()` con una expresión regular y el operador `Regex()`. Se pueden agregar varios patrones para eliminar diversos caracteres en una sola cadena de código.

## Ver también:
- Documentación oficial de Kotlin para `replace()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html
- Tutorial sobre expresiones regulares en Kotlin: https://www.geeksforgeeks.org/regular-expressions-in-kotlin/
- Ejemplos de eliminación de caracteres con patrón en Java: https://www.baeldung.com/java-remove-character-string