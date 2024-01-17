---
title:                "Uniendo cadenas"
html_title:           "Kotlin: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La concatenación de cadenas es cuando un programador combina diferentes cadenas de texto para crear una nueva cadena más larga. Esto se utiliza comúnmente para crear mensajes de salida personalizados o para agregar información adicional a una cadena existente. Los programadores hacen esto para crear salidas más dinámicas y personalizadas en sus programas.

## ¿Cómo hacerlo?

Puedes concatenar cadenas de texto en Kotlin usando el operador de suma (+) o el método "plus" que se puede llamar directamente en una cadena. Aquí hay un ejemplo de cómo hacerlo:

```Kotlin
val nombre = "Juan"
val apellido = "Pérez"

val nombreCompleto = nombre + " " + apellido
println(nombreCompleto)

// Salida: Juan Pérez
```

También puedes usar la plantilla de cadenas (string templates) en Kotlin para simplificar tu código. Aquí hay un ejemplo:

```Kotlin
val edad = 25
val mensaje = "Tengo $edad años de edad."
println(mensaje)

// Salida: Tengo 25 años de edad.
```
## Profundizando

La concatenación de cadenas es una práctica común en la programación y ha sido utilizada desde los primeros días de los lenguajes de programación. En algunos lenguajes, también se puede utilizar el método "concat" para concatenar cadenas o el operador de concatenación (.) para unir varias cadenas.

Otra manera de concatenar cadenas en Kotlin es utilizando la clase StringBuilder. Esta clase te permite construir cadenas sin crear y descartar objetos cada vez que agregas una cadena, lo que puede mejorar el rendimiento de tu programa.

## Ver también

Si quieres aprender más acerca de la concatenación de cadenas en Kotlin, puedes revisar la documentación oficial en [https://kotlinlang.org/docs/reference/basic-types.html#strings](https://kotlinlang.org/docs/reference/basic-types.html#strings).

También puedes consultar este artículo de Medium para aprender cómo utilizar la plantilla de cadenas en Kotlin: [https://medium.com/@eyeminers/rate-your-age-in-kotlin-d08a3c56d4e4](https://medium.com/@eyeminers/rate-your-age-in-kotlin-d08a3c56d4e4).

¡Ahora estás listo para empezar a concatenar cadenas en tus programas en Kotlin! ¡Diviértete y sigue aprendiendo!