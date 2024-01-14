---
title:                "Kotlin: Convirtiendo una cadena a minúsculas"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador en Kotlin, entonces probablemente estés familiarizado con el concepto de "cadena" (string). Las cadenas son un tipo de datos muy común y se utilizan en una variedad de aplicaciones. A veces, es posible que necesites convertir una cadena a minúsculas por razones de comparación o presentación. Este proceso puede ser útil en muchas situaciones, y en este artículo, exploraremos cómo convertir una cadena a minúsculas en Kotlin.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Kotlin, podemos utilizar el método `toLowerCase()` de la clase `String`. Este método toma la cadena original y devuelve una nueva cadena con todos los caracteres en minúsculas. Veamos un ejemplo:

```Kotlin
fun main() {
    val cadena = "Hola Mundo!"
    println(cadena.toLowerCase()) // salida: hola mundo!
}
```

Como se puede ver, usamos el método `toLowerCase()` en la cadena original `'Hola Mundo!'` y obtuvimos la cadena en minúsculas `'hola mundo!'` como resultado. Este método es útil cuando necesitas comparar cadenas sin importar si están en mayúsculas o minúsculas, ya que el método `equals()` es sensible a mayúsculas y minúsculas.

## Profundizando

Además del método `toLowerCase()`, Kotlin también ofrece el método `toCharArray()` que convierte la cadena en un array de caracteres. Luego, podemos recorrer este array y convertir cada carácter a minúsculas utilizando la función `toLowerCase()` de la clase `Char`. Veamos un ejemplo:

```Kotlin
fun main() {
    val cadena = "Hola Mundo!"
    val array = cadena.toCharArray()
    array.forEach { c -> print(c.toLowerCase()) } // salida: hola mundo!
}
```

Este enfoque es útil si necesitas hacer más manipulaciones con los caracteres individuales de la cadena.

## Ver también

Aquí hay algunos enlaces útiles que pueden ser de interés para ti:

- [Documentación oficial de Kotlin sobre cadenas](https://kotlinlang.org/docs/reference/strings.html)
- [Método `toLowerCase()` de la clase `String`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Método `toCharArray()` de la clase `String`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-char-array.html)