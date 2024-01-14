---
title:                "Kotlin: Encontrando la longitud de una cadena"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##¿Por qué encontrar la longitud de una cadena en Kotlin?

En la programación, puede ser necesario saber la longitud de una cadena de texto en diferentes situaciones. Puede ser para verificar si la entrada de un usuario cumple con ciertos requisitos de longitud, o para manipular una cadena de manera que solo una parte específica se use en la lógica del programa. En cualquier caso, conocer cómo encontrar la longitud de una cadena en Kotlin es una habilidad importante para cualquier desarrollador.

##Cómo encontrar la longitud de una cadena en Kotlin

Para encontrar la longitud de una cadena en Kotlin, podemos utilizar la función `length()` que está disponible en el tipo `String`. Esta función devuelve un valor entero que representa la cantidad de caracteres en la cadena.

```Kotlin
val cadena = "Esta es una cadena de ejemplo"
val longitud = cadena.length()

println("La longitud de la cadena es $longitud")

// Output: La longitud de la cadena es 27
```

Si queremos encontrar la longitud de una cadena que está dentro de un objeto, podemos acceder a ella utilizando el operador `.` en combinación con la función `length()`.

```Kotlin
data class Persona(val nombre: String, val edad: Int)

val persona = Persona("Juan", 32)

println(persona.nombre.length())

// Output: 4
```

También podemos utilizar la función `length()` directamente en una cadena de texto sin tener que asignarla a una variable.

```Kotlin
val longitud = "¡Hola!".length()

println(longitud)

// Output: 6
```

##Profundizando en la función `length()`

La función `length()` es una propiedad en Kotlin, lo que significa que no se agrega un paréntesis al final de su nombre al llamarla. Esta propiedad se puede utilizar en diferentes tipos de datos, no solo en cadenas de texto.

Además de la función `length()`, también podemos utilizar la función `count()` en Kotlin para obtener la longitud de una cadena. La diferencia entre ambas es que `count()` también se puede utilizar en colecciones, como listas, mientras que `length()` solo se aplica a cadenas de texto.

##Ver también

- [Documentación oficial de Kotlin sobre la función `length()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html)
- [Stack Overflow - ¿Cuál es la diferencia entre `length()` y `count()` en Kotlin?](https://stackoverflow.com/questions/43194349/whats-the-difference-between-length-and-count-in-kotlin)