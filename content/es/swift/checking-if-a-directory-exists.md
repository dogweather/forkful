---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:58:39.071496-07:00
simple_title:         "Comprobando si existe un directorio"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Comprobar si existe un directorio es verificar si hay una carpeta en un lugar específico del sistema de archivos. Los programadores hacen esto para evitar errores al leer, escribir o modificar archivos, asegurándose de que el directorio deseado está presente antes de intentar operar en él.

## How to:
Swift nos da el `FileManager` para trabajar con el sistema de archivos. Aquí tienes un ejemplo práctico de cómo verificar la existencia de un directorio:

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/ruta/al/directorio"

if fileManager.fileExists(atPath: directoryPath, isDirectory: UnsafeMutablePointer<ObjCBool>.allocate(capacity: 1)) {
    print("¡El directorio existe!")
} else {
    print("El directorio no existe.")
}
```

Si corres esto con una ruta válida, verás:

```
¡El directorio existe!
```

Para rutas inválidas o no existentes, obtendrás:

```
El directorio no existe.
```

## Deep Dive
Antes de `FileManager`, tenía que recurrirse a métodos de C como `stat()` para obtener detalles del sistema de archivos. Hoy, `FileManager` de Foundation es la forma moderna, ofreciendo métodos para tareas de manejo de archivos y directorios.

Hay alternativas a `fileExists(atPath:)`, como trabajar con `URL` y el método `resourceValues(forKeys:)`, que puede proporcionar más detalles a través de `URLResourceValues`. Aunque `fileExists(atPath:)` es suficientemente bueno para una verificación rápida.

Detalles de implementación a tener en cuenta: `fileExists(atPath:)` puede no ser completamente seguro en ambientes de múltiples hilos si el sistema de archivos cambia entre la verificación y el siguiente uso del archivo.

## See Also
- Documentación de Apple sobre `FileManager`: [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- Guía de Apple para trabajar con rutas de archivos y URLs: [File System Programming Guide](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
- Tutorial de Swift sobre manejo del sistema de archivos: [Ray Wenderlich File Management Tutorial](https://www.raywenderlich.com/666-filemanager-class-tutorial-for-macos-getting-started-with-the-file-system)
