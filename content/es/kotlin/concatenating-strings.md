---
title:                "Kotlin: Uniendo cadenas"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Un aspecto esencial de cualquier lenguaje de programación es la capacidad de trabajar con cadenas de texto. Una de las operaciones más comunes en este sentido es la concatenación de cadenas, que consiste en unir dos o más cadenas en una sola. Aunque puede parecer una operación simple, hay varias razones por las que uno podría necesitar concatenar cadenas en Kotlin. Por ejemplo, para crear un mensaje personalizado, generar una cadena de consulta para una URL, o simplemente para formatear y presentar datos al usuario.

## Cómo hacerlo

Para concatenar cadenas en Kotlin, podemos utilizar el operador `+` o la función `plus()`. El operador `+` simplemente une dos cadenas, como se puede ver en el siguiente ejemplo:

```Kotlin
val nombre = "Juan"
val apellido = "García"
val nombreCompleto = nombre + " " + apellido
println(nombreCompleto) // Salida: Juan García
```

Alternativamente, podemos utilizar la función `plus()` para concatenar más de dos cadenas:

```Kotlin
val paginaWeb = "www.miweb.com"
val protocolo = "http://"
val url = protocolo.plus(paginaWeb)
println(url) // Salida: http://www.miweb.com
```

También es posible concatenar cadenas con variables o expresiones, como en el siguiente ejemplo:

```Kotlin
val cantidad = 10
val mensaje = "Tienes " + cantidad + " mensajes nuevos."
println(mensaje) // Salida: Tienes 10 mensajes nuevos.
```

## Profundizando

Es importante tener en cuenta que cada vez que concatenamos cadenas en Kotlin, se crea una nueva cadena en la memoria. Por lo tanto, si necesitamos concatenar varias cadenas dentro de un bucle, podría ser más eficiente utilizar la clase `StringBuilder`, que nos permite crear una cadena mutable y modificarla sin tener que generar una nueva cada vez.

```Kotlin
val nombres = listOf("Juan", "María", "José")
val mensaje = StringBuilder("Los nombres son: ")
nombres.forEach { nombre ->
    mensaje.append(nombre).append(", ")
}
println(mensaje) // Salida: Los nombres son: Juan, María, José,
```

Otro aspecto importante es el uso de la función `format()`, que nos permite formatear cadenas con valores de variables de una manera más eficiente que concatenar manualmente. Por ejemplo:

```Kotlin
val nombre = "Jaime"
val edad = 25
val mensaje = "%s tiene %d años.".format(nombre, edad)
println(mensaje) // Salida: Jaime tiene 25 años.
```

## Vea también

- [Documentación oficial de Kotlin sobre concatenación de cadenas](https://kotlinlang.org/docs/basic-types.html#strings)
- [Tutorial de concatenación de cadenas en Kotlin](https://www.baeldung.com/kotlin-string-concatenation)
- [Uso de la clase `StringBuilder` en Kotlin](https://www.geeksforgeeks.org/stringbuilder-class-in-kotlin/)
- [Más sobre la función `format()` en Kotlin](https://www.programiz.com/kotlin-programming/string-formatting)