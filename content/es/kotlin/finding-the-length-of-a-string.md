---
title:                "Encontrando la longitud de una cadena"
html_title:           "Kotlin: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
En programación, encontrar la longitud de una cadena (o string en inglés) se refiere a determinar el número de caracteres que contiene una determinada secuencia de caracteres. Esto es útil para realizar tareas como la validación de entradas de usuario o la manipulación de strings en algoritmos. 

## Cómo:
Kotlin ofrece dos formas sencillas de encontrar la longitud de una cadena: utilizando la propiedad `length` o el método `count()`. 

```Kotlin
val cadena = "¡Hola mundo!"

println(cadena.length)
// Output: 12

println(cadena.count())
// Output: 12
```

También se puede utilizar un bucle `for` para recorrer cada caracter de la cadena y contarlos uno por uno, pero esto puede ser más tedioso y propenso a errores.
```Kotlin
var length = 0

for (char in cadena) {
  length += 1
}

println(length)
// Output: 12
```

## Profundizando:
En el pasado, la forma de encontrar la longitud de una cadena era con el método `length()` en lenguajes como Java. Sin embargo, esto no era consistente con otras clases de la librería, por lo que se decidió cambiar a la propiedad `length` en Kotlin para mantener una sintaxis uniforme. Otra alternativa para contar los caracteres en una cadena es utilizando expresiones regulares.

Además de contar caracteres, también se puede utilizar la función `count()` especificando un criterio de conteo, por ejemplo, `count { it.isUpperCase() }` para contar solo los caracteres en mayúsculas. También se puede combinar con otras funciones como `filter` o `map` para contar solo ciertos caracteres o para realizar otras operaciones antes de contarlos.

## Ver también:
- [Documentación oficial de Kotlin sobre las funciones `length` y `count()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- [Tutorial de Kotlin para principiantes: Strings](https://www.geeksforgeeks.org/kotlin-strings/)
- [Expresiones regulares en Kotlin](https://kotlinlang.org/docs/tutorials/regular-expression-operations.html)