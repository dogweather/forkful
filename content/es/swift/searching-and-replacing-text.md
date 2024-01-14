---
title:    "Swift: Buscando y reemplazando texto"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## ¿Por qué utilizar la búsqueda y reemplazo de texto en Swift?

La búsqueda y reemplazo de texto es una tarea común y necesaria en la programación. Nos permite encontrar y modificar texto específico en nuestros códigos. Esto puede ser útil para corregir errores, actualizar información o realizar cambios masivos en nuestro código. En Swift, hay varias formas de realizar esta tarea, por lo que es importante conocerlas.

## Cómo hacer la búsqueda y reemplazo de texto en Swift

Existen dos métodos principales para realizar la búsqueda y reemplazo de texto en Swift: utilizando cadenas de texto y utilizando expresiones regulares.

### Búsqueda y reemplazo utilizando cadenas de texto
El método más sencillo es utilizar las funciones `replacingOccurrences()` y `replacingOccurrences(of:with:)` de la clase `String`. Estas funciones toman como parámetros la cadena de texto que se quiere reemplazar y la cadena de texto que se quiere insertar en su lugar.

```Swift
let texto = "Hola Mundo"
let nuevoTexto = texto.replacingOccurrences(of: "Mundo", with: "Amigos") // "Hola Amigos"
```

También se pueden utilizar estos métodos junto con los operadores de asignación `+=` y `-=` para realizar reemplazos en cadenas de texto ya existentes.

```Swift
var texto = "Hola Mundo"
texto += " y Amigos" // "Hola Mundo y Amigos"
texto -= " y Amigos" // "Hola Mundo"
```

### Búsqueda y reemplazo utilizando expresiones regulares
Otra forma de realizar la búsqueda y reemplazo de texto es utilizando expresiones regulares. Estas nos permiten realizar búsquedas más complejas y precisas. Para utilizar expresiones regulares en Swift, es necesario importar la librería `Foundation`.

```Swift
import Foundation

let texto = "¡Hola 2022!"
let patron = " *[0-9]*" // Expresión regular que busca cualquier número
let nuevoTexto = texto.replacingOccurrences(of: patron, with: "", options: .regularExpression) // "¡Hola!"
```

## Profundizando en la búsqueda y reemplazo de texto en Swift

Aunque los ejemplos anteriores son útiles y cubren la mayoría de los casos, hay más cosas que se pueden hacer con la búsqueda y reemplazo de texto en Swift. Es importante explorar la documentación oficial y aprender más sobre los distintos métodos y opciones que ofrece el lenguaje.

## Ver también

- [Documentación oficial de Swift en búsquedas y reemplazos de texto](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID337)
- [Swift: cómo utilizar cadenas de texto y expresiones regulares](https://www.appcoda.com/swift-regular-expressions/)