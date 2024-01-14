---
title:    "Swift: Concatenando cadenas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica fundamental en la programación que permite combinar dos o más cadenas de texto para formar una sola. Esta habilidad es esencial para poder crear aplicaciones y programas que muestren información de manera legible y coherente.

## Cómo hacerlo

Hay varias formas de concatenar cadenas en Swift. Una de ellas es utilizando el operador `+`, que une dos cadenas para formar una nueva. Por ejemplo:

```Swift
let nombre = "Carlos"
let apellido = "García"
let nombreCompleto = nombre + " " + apellido
print(nombreCompleto)
```

Este código producirá la salida "Carlos García" en la consola. También puedes utilizar el método `append()` para agregar una cadena al final de otra:

```Swift
var mensaje = "Hola"
mensaje.append(", ¿cómo estás?")
print(mensaje)
```

La salida será "Hola, ¿cómo estás?". Otro método útil es `joined(separator:)`, que te permite unir múltiples cadenas con un separador específico:

```Swift
let palabras = ["Hola", "amigos", "!", "Bienvenidos"]
let mensaje = palabras.joined(separator: " ")
print(mensaje)
```

Esta vez, la salida será "Hola amigos ! Bienvenidos".

## Profundizando

Además de estas formas básicas de concatenar cadenas, también puedes utilizar el método `format()` para incluir variables dentro de una cadena. Este método utiliza marcadores de posición para indicar dónde se deben insertar las variables. Por ejemplo:

```Swift
let nombre = "Ana"
let edad = 24
let mensaje = "Hola, mi nombre es \(nombre) y tengo \(edad) años."
print(mensaje)
```

La salida será "Hola, mi nombre es Ana y tengo 24 años."

Puedes utilizar diferentes tipos de datos para rellenar los marcadores de posición, como números enteros, decimales y booleans. También puedes formatear el resultado final utilizando patrones de formato.

## Ver también

- Documentación oficial de Swift sobre concatenación de cadenas: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Tutorial sobre concatenación de cadenas en Swift: https://www.hackingwithswift.com/syntax/2-string-basics
- Ejemplos de concatenación de cadenas en diferentes escenarios: https://codewithchris.com/how-to-concatenate-strings-in-swift/