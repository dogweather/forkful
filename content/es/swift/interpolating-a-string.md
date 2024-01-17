---
title:                "Interpolando una cadena"
html_title:           "Swift: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Interpolar una cadena es básicamente insertar valores variables en una cadena de texto. Los programadores lo hacen para crear cadenas dinámicas y más fáciles de leer en su código.

## Cómo:
Este es un ejemplo de cómo interpolar una cadena en Swift:

```Swift
let nombre = "Juan"
let edad = 25

let mensaje = "¡Hola, mi nombre es \(nombre) y tengo \(edad) años!"
print(mensaje)

// Output: ¡Hola, mi nombre es Juan y tengo 25 años!
```

## Exploración en profundidad:
La interpolación de cadenas se introdujo en Swift en su versión 2.0 como una forma de reemplazar la antigua función `sprintf` utilizada en otros lenguajes de programación. Otras alternativas para crear cadenas dinámicas incluyen el uso de `String(format:)` y `String(describing:)`. 

La interpolación de cadenas se implementa en Swift utilizando el operador de interpolación `\()` y puede contener cualquier expresión válida. Esto significa que puede interpolar no solo valores de variables, sino también resultados de funciones y operaciones aritméticas.

## Ver también:
- Documentación oficial de Swift sobre la interpolación de cadenas: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID303
- Tutorial en español sobre interpolación de cadenas en Swift: https://latam.ek.ua/tutoriales/2009-10-04/interpolar-cadenas-con-swift/