---
title:                "Escribiendo en el error estándar"
html_title:           "Swift: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has encontrado un error en tu código y no has podido averiguar por qué está sucediendo? Eso puede ser frustrante y llevar mucho tiempo si tienes que buscarlo en tu código. Es por eso que escribir a la salida estándar de error puede ser una herramienta útil para depurar tu código y ayudarte a encontrar esos errores más rápidamente.

## Cómo hacerlo

Una forma de escribir a la salida estándar de error en Swift es utilizando la función `print(_:to:)`. Esta función toma como primer argumento el valor que quieres imprimir y como segundo argumento el objeto en el que quieres imprimirlo. En este caso, queremos imprimir a la salida estándar de error, por lo que utilizamos `StandardError.self` como segundo argumento.

```Swift
let error = "Este es un mensaje de error"
print(error, to: .standardError)
```

El resultado de este código sería:

`Este es un mensaje de error`

Otra forma de escribir a la salida estándar de error es utilizando el objeto `FileHandle.standardError`. Este objeto representa la salida estándar de error y permite escribir en ella utilizando el método `write(_:)`. El siguiente ejemplo muestra cómo escribir un mensaje de error utilizando este método.

```Swift
if let errorData = "Este es un mensaje de error".data(using: .utf8) {
    FileHandle.standardError.write(errorData)
}
```

El resultado de este código sería el mismo que el anterior, un mensaje de error impreso en la salida estándar de error.

## Profundizando en el tema

Al escribir a la salida estándar de error, es importante tener en cuenta que es diferente a la salida estándar de la consola. La salida estándar de error es utilizada específicamente para mostrar errores y debería ser utilizado solo para ese propósito. Además, es importante no abusar de esta herramienta y solo escribir mensajes relevantes para ayudar a depurar tu código.

También puedes utilizar la función `debugPrint(_:to:)` para imprimir en la salida estándar de error. Esta función es útil para imprimir el valor de una variable o constante junto con su nombre, lo que puede ser útil para saber qué parte de tu código está causando un error.

```Swift
let num = 6
debugPrint(num, to: .standardError)
// Output: "num: 6"
```

Además, si estás desarrollando una aplicación de línea de comandos, puedes utilizar la opción `-v` al ejecutar tu aplicación para imprimir mensajes de depuración en la consola y en la salida estándar de error.

## Ver también

- [Documentación oficial de Swift sobre la función `print(_:to:)`](https://developer.apple.com/documentation/swift/standardlibrary/output)
- [Guía de uso de la salida estándar de error en Swift](https://www.swiftbysundell.com/basics/standard-error/)
- [Opciones de depuración en aplicaciones de línea de comandos en Swift](https://www.hackingwithswift.com/articles/114/how-to-debug-errors-in-your-command-line-swift-programs)