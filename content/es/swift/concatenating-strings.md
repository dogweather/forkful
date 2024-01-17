---
title:                "Concatenando cadenas"
html_title:           "Swift: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Concatenar cadenas simplemente significa unir dos o más cadenas en una sola. Los programadores a menudo lo hacen para crear una cadena más larga o para combinar variables con texto para formatear una salida.

## Cómo:

Aquí hay un ejemplo simple de cómo concatenar cadenas en Swift utilizando el operador de adición (+) y la palabra clave "string":

```Swift
let nombre = "Juan"
let apellido = "Pérez"
let nombreCompleto = nombre + " " + apellido
print(nombreCompleto) // Output: "Juan Pérez"
```

Ya que las cadenas se pueden tratar como secuencias de caracteres en Swift, también puedes usar el método de cadena `joined()` para concatenar varias cadenas en lugar del operador de adición:

```Swift
let saludo = "¡Hola!"
let conjunto = ["Buenos", "días"]
let saludoCompleto = saludo + " " + conjunto.joined(separator: " ")
print(saludoCompleto) // Output: "¡Hola! Buenos días"
```

## Buceo Profundo:

En el pasado, concatenar cadenas era una tarea tediosa y propensa a errores en los lenguajes de programación, ya que se tenían que manejar los espacios vacíos y otros caracteres especiales manualmente. Sin embargo, en Swift, la sintaxis y los métodos incorporados hacen que esta tarea sea más fácil y menos propensa a errores.

Además de usar el operador de adición y el método `joined()`, también puedes utilizar la función `String(describing:)` para convertir valores de otros tipos en cadenas y concatenarlos con otras cadenas. Por ejemplo:

```Swift
let edad = 20
let mensaje = "Tengo " + String(describing: edad) + " años"
print(mensaje) // Output: "Tengo 20 años"
```

Hay alternativas a la concatenación de cadenas en Swift, como el método `append()` para agregar cadenas a una cadena existente y el método `appendFormat()` para agregar texto formateado a una cadena. Sin embargo, para la mayoría de los casos, el uso del operador de adición y el método`joined()` será suficiente.

## Ver también:

- [Documentación oficial de Swift sobre cadenas](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Ejemplos de concatenación de cadenas en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-combine-strings-together)