---
title:    "Kotlin: Encontrar la longitud de una cadena"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## ¿Por qué?

A menudo, en programación, necesitamos saber la longitud de una cadena de texto. Ya sea para validar una entrada del usuario, realizar operaciones matemáticas o simplemente por fines de visualización, conocer la longitud de una cadena puede ser una tarea útil en muchos escenarios.

## Cómo hacerlo

En Kotlin, podemos encontrar la longitud de una cadena fácilmente utilizando la función `length()` que viene integrada en la clase `String`. Veamos un ejemplo práctico:

```Kotlin
fun main() {
    val frase = "Hola, ¿cómo estás?"
    val longitud = frase.length()

    println("La longitud de la frase es $longitud")
}
```

La salida de este código será:

```
La longitud de la frase es 18
```

También podemos utilizar la propiedad `length` directamente en la cadena, en lugar de llamar a la función `length()`, ya que en Kotlin las propiedades y las funciones son intercambiables para obtener valores.

```Kotlin
val longitud = frase.length
```

Por último, es importante mencionar que la función `length()` cuenta todos los caracteres de la cadena, incluyendo espacios en blanco, signos de puntuación, y acentos.

## Profundizando

Si nos adentramos un poco más en la función `length()`, podemos descubrir que en realidad es una extensión de la clase `CharSequence`, la cual representa un conjunto de caracteres que se pueden acceder secuencialmente. Esto significa que también podemos obtener la longitud de otras clases como `StringBuilder`, `StringBuffer` y `CharArray`.

Además, la función `length()` en realidad es una delegación a la propiedad `size` de la clase `CharSequence`, lo que nos permite acceder al tamaño de una cadena sin necesidad de llamar a una función.

Por último, si necesitamos una manera más eficiente de obtener la longitud de una cadena, podemos utilizar el operador `count()` en lugar de la función `length()`. El uso de `count()` es especialmente útil en cadenas grandes, ya que evita la creación de una nueva instancia de `Int` para almacenar el valor de la longitud.

## Ver también 

- [Documentación oficial de Kotlin sobre la clase `CharSequence`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-char-sequence/)
- [Documentación oficial de Kotlin sobre la función `length()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html)
- [Ejemplo práctico de uso de la función `length()`](https://www.programiz.com/kotlin-programming/strings#length)

¡Con esto ya estás listo para utilizar la función `length()` en tus proyectos y obtener la longitud de tus cadenas de forma sencilla y eficiente! Espero que este artículo te haya sido útil. ¡Hasta la próxima!