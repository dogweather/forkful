---
title:    "Swift: Creando un archivo temporal"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué
Crear un archivo temporal en la programación puede ser útil en varias situaciones, como guardar datos temporalmente antes de ser procesados o para almacenar información que no requiere ser guardada permanentemente en el dispositivo.

## Cómo
Para crear un archivo temporal en Swift, utilizamos la clase `FileManager` y su método `temporaryDirectory`. Luego, podemos crear un archivo en ese directorio con el nombre y extensión que deseemos.

```Swift
let fileManager = FileManager.default
let temporaryDirectory = fileManager.temporaryDirectory
let temporaryFile = temporaryDirectory.appendingPathComponent("example.txt")

// Sample output: file:///private/var/folders/fz/dcrm4jkd36ngp3g5z6__ty2h0000gn/T/example.txt
```

## Profundizando
Para comprender mejor el proceso de creación de archivos temporales en Swift, es importante tener en cuenta que estos archivos están diseñados para ser utilizados solo temporalmente y pueden ser eliminados por el sistema operativo en cualquier momento. Por lo tanto, no se recomienda utilizarlos para almacenar información importante.

Sin embargo, podemos especificar algunas opciones al crear un archivo temporal. Por ejemplo, podemos establecer la opción `isExpendable` en `true` para indicar que el archivo puede ser eliminado por el sistema en caso de que se necesite espacio en disco. También podemos utilizar la opción `deleteUponClose` para indicar que el archivo debe eliminarse automáticamente cuando se cierre.

```Swift
var options = [URLWritingOptionsKey : Any]()
options[.isExpendable] = true
options[.deleteUponClose] = true
let temporaryFile = temporaryDirectory.appendingPathComponent("example.txt")
FileManager.default.createFile(atPath: temporaryFile.path, contents: Data(), attributes: nil, options: options)
```

## Ver también
- Documentación de Apple sobre FileManager: <https://developer.apple.com/documentation/foundation/filemanager>
- Tutorial de Ray Wenderlich sobre archivos temporales en Swift: <https://www.raywenderlich.com/840-swift-tutorial-how-to-use-nsurlsession>
- Código de ejemplo de creación de archivos temporales en Swift: <https://gist.github.com/yudai/609a94081814dd4f3159>