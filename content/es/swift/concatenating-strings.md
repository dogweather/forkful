---
title:    "Swift: Concatenando cadenas."
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica muy útil en programación Swift. Esta permite unir varias cadenas de texto en una sola, facilitando la tarea de mostrar información al usuario. En esta publicación, te mostraremos cómo puedes utilizar la concatenación de cadenas en tus proyectos de forma sencilla y efectiva.

## Cómo hacerlo

En Swift, la concatenación de cadenas se realiza utilizando el operador `+`. Por ejemplo:

```Swift
let nombre = "María"
let saludo = "¡Hola, " + nombre + "!"
print(saludo)

// Output: ¡Hola, María!
```

También puedes utilizar el operador `+=` para modificar una cadena existente:

```Swift
var saludos = "¡Hola!"
saludos += " ¿Cómo estás?"
print(saludos)

// Output: ¡Hola! ¿Cómo estás?
```

Además, puedes combinar diferentes tipos de datos utilizando la concatenación de cadenas. Por ejemplo:

```Swift
let edad = 26
let descripcion = "Tengo " + String(edad) + " años."
print(descripcion)

// Output: Tengo 26 años.
```

## Profundizando

Hay algunos detalles importantes que debes tener en cuenta al utilizar la concatenación de cadenas en Swift. Por ejemplo, si quieres concatenar un texto con un número, es necesario convertir el número a una cadena de texto utilizando `String()`. De lo contrario, obtendrás un error.

Otra cosa a tener en cuenta es que los operadores `+` y `+=` solo funcionan con cadenas de texto y no con otros tipos de datos, como por ejemplo enteros o decimales.

También puedes utilizar la interpolación de cadenas para insertar valores de variables en una cadena de texto. Esta técnica es muy útil y puedes ver cómo se utiliza en nuestro [tutorial sobre la interpolación de cadenas en Swift](https://www.misitioweb.com/tutorial-interpolacion-cadenas-swift).

## Ver también

- [Tutorial sobre la interpolación de cadenas en Swift](https://www.misitioweb.com/tutorial-interpolacion-cadenas-swift)
- [Documentación oficial de Swift sobre la concatenación de cadenas](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID523)
- [Ejemplos de concatenación de cadenas en Swift](https://www.avanderlee.com/swift/string-interpolation-swift-5/)