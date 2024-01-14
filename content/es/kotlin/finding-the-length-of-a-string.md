---
title:                "Kotlin: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# ¿Por qué deberías encontrar la longitud de una cadena en Kotlin?

En la programación, a menudo te encontrarás trabajando con cadenas de texto y necesitarás saber su longitud. Ya sea para realizar algún tipo de validación o para formatear la salida de datos, conocer la longitud de una cadena puede ser una tarea muy útil. En esta entrada, aprenderás cómo encontrar la longitud de una cadena en Kotlin y profundizarás en cómo funciona este concepto.

## Cómo hacerlo

En Kotlin, se puede obtener la longitud de una cadena utilizando la función `length()`. Esta función es parte de la clase `String` y devuelve un entero que representa el número de caracteres en la cadena. Veamos un ejemplo de cómo utilizarlo:

```Kotlin
val miCadena = "¡Hola mundo!"
val longitudCadena = miCadena.length()

println("La longitud de la cadena es $longitudCadena") // La longitud de la cadena es 12
```

En este ejemplo, creamos una variable `miCadena` que contiene la cadena "¡Hola mundo!". Luego, utilizamos la función `length()` para obtener su longitud y la almacenamos en la variable `longitudCadena`. Finalmente, imprimimos el resultado utilizando la función `println()`.

También es posible obtener la longitud de una cadena utilizando el operador de acceso `[ ]`, que permite acceder a un carácter específico en una cadena mediante su índice. El índice de la primera letra de una cadena es 0 y el de la última letra es la longitud de la cadena menos 1. Por lo tanto, el índice del último carácter de la cadena es `longitudCadena - 1`. Veamos un ejemplo:

```Kotlin
val miCadena = "¡Hola mundo!"
val longitudCadena = miCadena[0] // Obtener el primer carácter
val ultimoIndice = miCadena.length() - 1
val ultimoCaracter = miCadena[ultimoIndice] // Obtener el último carácter

println("La primera letra de la cadena es $longitudCadena y la última es $ultimoCaracter") 
// La primera letra de la cadena es ¡ y la última es !
```

## Profundizando

Cuando se trabaja con cadenas en Kotlin, es importante tener en cuenta que cada carácter ocupa un espacio en la memoria. Esto significa que la longitud de una cadena no necesariamente indica cuántos bytes ocupa en la memoria. Por ejemplo, en el caso de una cadena que contiene caracteres acentuados o en otros idiomas, un solo carácter puede ocupar más de un byte.

Además, es importante tener en cuenta que, al igual que en otros lenguajes de programación, en Kotlin una cadena es inmutable, lo que significa que no se puede modificar después de ser creada. Por lo tanto, si necesitas cambiar una cadena, debes crear una nueva en lugar de modificar la existente.

## Ver también

- [Documentación oficial de Kotlin sobre cadenas](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial de Kotlin: tipos de datos básicos](https://www.tutorialspoint.com/kotlin/kotlin_basic_types.htm)
- [Ejemplos de cadenas en Kotlin](https://www.programiz.com/kotlin-programming/string)