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

## ¿Por qué?

 La longitud de una cadena es fundamental en la programación ya que nos permite conocer la cantidad de caracteres que contiene una determinada cadena de texto. Esto es especialmente útil cuando trabajamos con cadenas de entrada del usuario o cuando necesitamos realizar operaciones específicas según la longitud de una cadena.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Kotlin, podemos utilizar el método `length()` que se encuentra en la clase `String`. Este método devuelve un entero con la cantidad de caracteres de la cadena. Veamos un ejemplo:

```Kotlin
val cadena = "¡Hola, mundo!"
println(cadena.length()) // Imprime: 13
```

En este caso, la variable `cadena` contiene la cadena de texto "¡Hola, mundo!" y al llamar al método `length()` obtenemos como resultado el número 13, que es la cantidad de caracteres de la cadena.

También podemos utilizar el método `count()` que realiza una operación similar y devuelve la misma cantidad de caracteres, pero se puede utilizar con cadenas de texto más largas y complejas. Veamos otro ejemplo:

```Kotlin
val cadenaLarga = "Este es un texto largo con muchas palabras."
println(cadenaLarga.count()) // Imprime: 43
```

En este caso, el método `count()` ha contado todos los caracteres, incluyendo espacios en blanco y signos de puntuación. Esto es útil cuando necesitamos realizar operaciones más avanzadas con una cadena.

## Profundizando

Para aquellos interesados en conocer más sobre cómo funcionan estos métodos en Kotlin, es importante tener en cuenta que la mayoría de los caracteres en una cadena ocupan un solo espacio, excepto cuando se trata de emojis o de ciertos caracteres acentuados. Por ejemplo, en Kotlin la letra ñ es un solo carácter, mientras que en otros lenguajes puede ser considerada como dos caracteres separados.

También es importante destacar que estos métodos son muy útiles cuando trabajamos con bucles o condiciones, ya que nos permiten obtener la longitud de una cadena de manera rápida y sencilla.

## Ver también

- [Documentación oficial de Kotlin sobre la clase String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Ejemplos prácticos de cómo trabajar con cadenas en Kotlin](https://www.geeksforgeeks.org/kotlin-sequence-stringlength-method/)