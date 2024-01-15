---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Kotlin: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Convertir una cadena de texto a minúsculas puede ser útil en varias situaciones, por ejemplo, al comparar cadenas de forma sensible a mayúsculas y minúsculas o al limpiar y normalizar datos antes de realizar operaciones en una base de datos.

## Cómo hacerlo

```kotlin
// Crear una variable con una cadena de texto
var cadena = "Hola Mundo!"

// Utilizar el método toLowerCase() para convertir a minúsculas
var cadenaMinuscula = cadena.toLowerCase()

// Imprimir la cadena en minúsculas
println(cadenaMinuscula)
```

**Output:** hola mundo!

## Un vistazo más profundo

Para convertir una cadena a minúsculas, Kotlin utiliza el estándar Unicode. Esto significa que, además de las letras del alfabeto, también se pueden convertir símbolos y caracteres especiales a minúsculas. Además, el método toLowerCase() es una operación inmutable, lo que significa que no modifica la cadena original, sino que devuelve una nueva cadena con los cambios aplicados.

Otra forma de convertir una cadena a minúsculas es utilizando la función de extensión `toLowerCase()`:

```kotlin
var cadenaMinuscula = cadena.toLowerCase()
```

Esto es útil si se trabaja con cadenas de texto largas, ya que la sintaxis es más simple y legible.

## Ver también

- [Documentación oficial de Kotlin sobre la función toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Funciones de extensión en Kotlin](https://kotlinlang.org/docs/reference/extensions.html)