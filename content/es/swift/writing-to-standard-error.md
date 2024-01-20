---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir en el error estándar (stderr) permite separar los mensajes de error de la salida normal (stdout). Los programadores lo hacen para diagnosticar problemas sin mezclar con los datos de salida, facilitando el manejo y depuración de los programas.

## Cómo hacerlo:

Para escribir en stderr en Swift, usa `FileHandle.standardError` y el método `write(_:)`. Aquí un ejemplo:

```Swift
import Foundation

func writeToStandardError() {
    if let errorMessage = "Ocurrió un error.\n".data(using: .utf8) {
        FileHandle.standardError.write(errorMessage)
    }
}

writeToStandardError()
```

Output en la consola será visible solo si rediriges stderr, como `> /dev/null 2>&1`.

## Análisis Profundo:

Históricamente, stderr se ha utilizado para permitir que los mensajes de error sean vistos o manejados diferentemente de la salida estándar (stdout). Otros métodos, como utilizar `NSLog` o `print(_:to:)`, existen pero tienen diferentes propósitos o implicaciones. Por ejemplo:
- `NSLog` se usa más en ambientes de Apple y agrega metadata automáticamente.
- `print(_:to:)` permite especificar un stream de salida, como stderr.

La implementación directa con `FileHandle` es poderosa y flexible. Los errores pueden ser redirigidos, registrados en archivos o manejados por otros programas.

## Ver También:

- Documentación oficial de Swift, `FileHandle`: https://developer.apple.com/documentation/foundation/filehandle
- Guía sobre el manejo del output en la programación: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- Discusión sobre stderr en Stack Overflow: https://stackoverflow.com/questions/9050260/what-does-a-single-ampersand-mean-in-the-shell-syntax
- Artículo sobre la historia de Unix, stderr: https://www.bell-labs.com/usr/dmr/www/hist.html