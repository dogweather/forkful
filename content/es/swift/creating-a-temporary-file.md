---
title:                "Creando un archivo temporal"
html_title:           "Swift: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por Qué
¿Alguna vez has tenido la necesidad de crear un archivo temporal en tus programas? Quizás necesites almacenar información temporalmente antes de guardarla permanentemente, o tal vez quieres practicar código que requiere la creación de archivos temporales. En este artículo, veremos cómo crear archivos temporales en Swift y por qué puede ser útil hacerlo.

## Cómo Hacerlo
Crear un archivo temporal en Swift es muy sencillo. Primero, necesitamos importar la librería Foundation en nuestro archivo. Luego, utilizamos la función `NSTemporaryDirectory()` para obtener el directorio temporal del sistema. A continuación, podemos utilizar este directorio junto con el nombre y extensión de nuestro archivo temporal para crear un objeto de tipo `URL`. Por último, podemos escribir nuestro contenido en el archivo utilizando la función `write(to:, atomically:, encoding:)`.

```Swift
import Foundation

let temporaryDirectory = NSTemporaryDirectory()
let temporaryFilePath = temporaryDirectory.appendingPathComponent("tempFile.txt")
let fileURL = URL(fileURLWithPath: temporaryFilePath)
let content = "Este es un archivo temporal en Swift"
try? content.write(to: fileURL, atomically: true, encoding: .utf8)
```

Este código creará un archivo temporal llamado "tempFile.txt" en el directorio temporal del sistema y escribirá el texto "Este es un archivo temporal en Swift" en él.

## Profundizando
Ahora que sabemos cómo crear un archivo temporal en Swift, es importante entender qué sucede detrás de escena y cómo podemos modificar este proceso. Cuando utilizamos la función `NSTemporaryDirectory()`, obtenemos el directorio que el sistema operativo designa para almacenar archivos temporales. Sin embargo, también podemos especificar nuestro propio directorio utilizando la función `URL(fileURLWithPath:)` y proporcionando una ruta personalizada.

Además, al utilizar la función `write(to:, atomically:, encoding:)`, podemos establecer el parámetro `atomically` en `false` para escribir directamente el contenido en el archivo sin guardar una copia en una ubicación distinta y renombrarla en caso de que surja algún error. Esto puede ser útil si queremos mantener nuestra información totalmente privada.

## Ver También
- [Documentación de la clase `NSTemporaryDirectory()`](https://developer.apple.com/documentation/foundation/1409212-nstemporarydirectory)
- [Documentación de la clase `URL`](https://developer.apple.com/documentation/foundation/url)
- [Documentación de la función `write(to:, atomically:, encoding:)`](https://developer.apple.com/documentation/foundation/nsstring/1410351-write)