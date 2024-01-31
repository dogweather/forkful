---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:51:57.036827-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
La interpolación de cadenas permite insertar variables, constantes y expresiones dentro de una cadena de texto. Los programadores la usan para crear mensajes dinámicos y fáciles de mantener.

## Cómo hacerlo:
Vamos a verlo con código. Añade variables en una cadena usando `\(variable)`. Aquí tienes algunos ejemplos:

```Swift
let nombre = "Miguel"
let edad = 30
let mensaje = "Hola, mi nombre es \(nombre) y tengo \(edad) años."
print(mensaje)
```

Salida:
```
Hola, mi nombre es Miguel y tengo 30 años.
```

Si necesitas algo más complejo, como una operación matemática:

```Swift
let precio = 9.99
let cantidad = 3
let mensajeCompra = "El total es \(precio * Double(cantidad)) euros."
print(mensajeCompra)
```

Salida:
```
El total es 29.97 euros.
```

## Profundizando
La interpolación de cadenas no es exclusiva de Swift. Otros lenguajes como Python o JavaScript la utilizan con sintaxis diferentes. En Swift, apareció con la primera versión en 2014 y resultó ser más intuitiva que el antiguo método de formateo NSString de Objective-C, que usaba tokens como `%@` o `%d`.

Además de insertar variables o constantes, puedes llamar funciones directamente dentro de los paréntesis interpolados. Por ejemplo, `\(miFuncion())` ejecutará `miFuncion` y colocará su resultado en la cadena.

Alternativamente, puedes usar el formateo de cadenas, pero es menos directo que la interpolación. Aquí tienes cómo se hace con `String(format:)`:

```Swift
let temperatura = 21.5
let mensajeTemperatura = String(format: "La temperatura actual es %.1f grados Celsius.", temperatura)
print(mensajeTemperatura)
```

Ejecutaría un poco más lento que la interpolación y es más propenso a errores debido a los especificadores de tipo.

## También verifica
- [Swift Docs sobre cadenas y caracteres](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Stack Overflow: Uso avanzado de interpolación de cadenas en Swift](https://stackoverflow.com/questions/tagged/swift+string-interpolation)
