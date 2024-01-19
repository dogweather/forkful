---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creación de archivos temporales en Swift

## ¿Qué y Por qué?
La creación de archivos temporales implica hacer un archivo útil sólo por un breve periodo de tiempo. Los programadores lo hacen para almacenar datos temporalmente o para probar ciertas funcionalidades.

## Cómo hacerlo:
Aquí te dejo un ejemplo de cómo crear un archivo temporal en Swift:

```Swift
import Foundation

let tempDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let targetURL = tempDirectoryURL.appendingPathComponent(UUID().uuidString)

do {
    try "Hola Mundo".write(to: targetURL, atomically: true, encoding: .utf8)
    print("Archivo escrito en \(targetURL.path)")
} catch {
    print("Error al escribir el archivo: \(error)")
}
```
Este código escribe "Hola Mundo" en un archivo temporal y luego imprime la ruta.

## Profundizando 
La creación de archivos temporales tiene muchas raíces en el pasado, su utilización se hizo común con los sistemas operativos Unix. Las alternativas a la creación de archivos temporales incluyen la creación de una base de datos en memoria o el uso de contenedores de datos en vivo. Cada enfoque tiene sus ventajas y desventajas, y el correcto dependerá fuertemente de tu caso de uso específico. Los archivos temporales en Swift son creados en la carpeta temporal del sistema, y se les asigna un nombre único basado en un UUID para evitar conflictos.

## Ver también
Para obtener más información, consulta los siguientes recursos:

1. [Documentación oficial de Apple sobre la Escritura y lectura de archivos](https://developer.apple.com/documentation/foundation/url/1408481-write)
2. [Discusión en StackOverflow sobre el manejo de archivos temporales en Swift](https://stackoverflow.com/questions/37956482/read-write-text-file-in-swift)
3. [NSHipster: una mirada a NSTemporaryDirectory()](https://nshipster.com/nstemporarydirectory/)