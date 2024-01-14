---
title:                "Kotlin: Capitalizando una cadena"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena

Capitalizar una cadena es una práctica común en la programación, ya que nos permite mostrar un texto en mayúsculas para enfatizarlo o simplemente por razones estéticas. También es útil al trabajar con bases de datos o al comparar cadenas de texto.

## Cómo capitalizar una cadena

Para capitalizar una cadena en Kotlin, podemos utilizar el método `capitalize()` que nos proporciona la clase `String`. Este método convierte el primer carácter de la cadena a mayúscula y deja el resto en minúsculas. Veamos un ejemplo:

```Kotlin
val cadena = "hola mundo"
println(cadena.capitalize()) // Salida: Hola mundo
```

También podemos utilizar el método `toUpperCase()` que convierte toda la cadena a mayúsculas, o `toLowerCase()` que la convierte a minúsculas, según sea necesario para nuestro caso de uso.

## Profundizando en la capitalización de cadenas

Hay varios aspectos a tener en cuenta al trabajar con la capitalización de cadenas. Por ejemplo, es importante tener en cuenta que estos métodos sólo afectan a los caracteres que corresponden a letras en su idioma, por lo que si tenemos caracteres especiales o símbolos, no serán transformados.

Además, también podemos utilizar el método `capitalizeWords()` para capitalizar cada palabra de una cadena, en lugar de sólo el primer carácter. Y si queremos personalizar la capitalización, podemos utilizar el método `replaceRange()` para reemplazar la primera letra de la cadena con una versión en mayúscula, y luego utilizar el método `substring()` para eliminar la primera letra original.

## Ver También

- [Documentación de Kotlin sobre la clase String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Tutorial de programación básica en Kotlin](https://www.geeksforgeeks.org/kotlin-programming-language-basic-syntax/)
- [Ejemplos de capitalización en Kotlin](https://www.programiz.com/kotlin-programming/capitalize-letters)

¡Con estas herramientas podrás capitalizar tus cadenas de forma eficiente y elegante en tus proyectos de Kotlin!