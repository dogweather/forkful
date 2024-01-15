---
title:                "Uniendo cadenas de texto"
html_title:           "Kotlin: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Hoy en día, la concatenación de cadenas es una tarea común en la programación. Permite unir varias cadenas en una sola, lo que es útil en situaciones como la creación de mensajes, la generación de nombres de archivos o la construcción de consultas de bases de datos.

## ¿Cómo hacerlo?

Se puede concatenar cadenas de varias formas en Kotlin. Una opción es usar el operador `+` para unir dos cadenas. Por ejemplo:

```Kotlin
val nombre = "María"
val apellido = "García"
val nombreCompleto = nombre + " " + apellido
println(nombreCompleto) // imprime "María García"
```

También se pueden utilizar plantillas de cadenas, que permiten insertar variables dentro de una cadena. Por ejemplo:

```Kotlin
val edad = 25
val mensaje = "Tengo $edad años"
println(mensaje) // imprime "Tengo 25 años"
```

Otra forma es utilizar la función `format()` para crear una cadena con un formato específico. Por ejemplo:

```Kotlin
val ciudad = "Madrid"
val temperatura = 20.5
val mensaje = "La temperatura en %s es %.1f grados".format(ciudad, temperatura)
println(mensaje) // imprime "La temperatura en Madrid es 20.5 grados"
```

## Un poco más profundo

En Kotlin, las cadenas son inmutables, lo que significa que no se pueden modificar una vez creadas. Por lo tanto, cada vez que se concatena una cadena, se crea una nueva en memoria. Esto puede causar problemas de rendimiento si se concatenan grandes cantidades de cadenas en un bucle, por lo que es mejor utilizar la clase `StringBuilder` en esos casos. Esta clase permite modificar una cadena sin crear una nueva en memoria, lo que mejora el rendimiento.

Además, es importante tener en cuenta que la concatenación de cadenas puede ser costosa en términos de recursos. Por esta razón, es recomendable utilizar plantillas de cadenas o la función `format()` en lugar del operador `+` cuando se concatena un gran número de cadenas o se necesita un rendimiento óptimo.

## Ver también

- [Documentación oficial de concatenación de cadenas en Kotlin](https://kotlinlang.org/docs/basic-types.html#strings)
- [Ejemplos de concatenación de cadenas en Kotlin](https://www.geeksforgeeks.org/kotlin-string-concatenation/)
- [Más información sobre la clase `StringBuilder` en Kotlin](https://www.baeldung.com/kotlin/stringbuilder)