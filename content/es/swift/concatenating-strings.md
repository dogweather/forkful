---
title:                "Uniendo cadenas de texto"
html_title:           "Swift: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de strings, o la unión de varias cadenas de texto en una sola, es una práctica común en la programación en Swift. Puede ser útil en situaciones como la creación de mensajes personalizados para el usuario o la construcción de una URL con parámetros específicos. En resumen, la concatenación de strings es una herramienta esencial para manejar texto en cualquier aplicación de Swift.

## Cómo hacerlo

Para concatenar strings en Swift, podemos utilizar el operador `+` o el método `append()`. Veamos algunos ejemplos de ambos en acción:

```Swift
// Utilizando el operador +
let nombre = "Juan"
let saludo = "¡Hola, " + nombre + "!"

print(saludo) // output: ¡Hola, Juan!

// Utilizando el método append()
let primerNombre = "María"
let segundoNombre = "Elena"
let nombreCompleto = primerNombre.append(" ").append(segundoNombre)

print(nombreCompleto) // output: María Elena
```

Como podemos ver, el operador `+` nos permite unir strings de forma sencilla, mientras que el método `append()` nos da más control sobre cómo queremos combinar nuestras cadenas de texto.

## Profundizando

Además de utilizar el operador `+` y el método `append()`, también podemos concatenar strings utilizando la interpolación de cadenas, utilizando el símbolo `\()`. Veamos un ejemplo:

```Swift
let edad = 25
let mensaje = "Tengo \(edad) años."

print(mensaje) // output: Tengo 25 años.
```

Aquí, el valor de `edad` se inserta directamente en el string utilizando la interpolación. Esto puede ser especialmente útil para crear mensajes personalizados utilizando datos variables.

También es importante tener en cuenta que, al concatenar strings, es importante asegurarnos de que estemos trabajando con el mismo tipo de datos. Por ejemplo, si intentamos unir un string con un número, obtendremos un error. En su lugar, podemos utilizar el método `String()` para convertir cualquier tipo de dato en un string antes de concatenarlo.

## Ver también

- [Documentación oficial de Apple sobre la concatenación de strings en Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID290)
- [Artículo en español sobre el uso de Strings en Swift](https://www.appcoda.com.mx/strings-swift/)