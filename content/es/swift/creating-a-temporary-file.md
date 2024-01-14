---
title:    "Swift: Creando un archivo temporal"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en Swift

Crear archivos temporales es una práctica común en la programación en Swift, especialmente cuando se trabaja con grandes cantidades de datos. Estos archivos son útiles porque te permiten almacenar temporalmente información mientras se está ejecutando un programa, y luego se elimina después de su uso. Esto puede ser útil para evitar la sobrecarga de memoria y mantener tu código organizado.

## Cómo crear un archivo temporal en Swift

Para crear un archivo temporal en Swift, primero necesitas importar la librería `Foundation` en tu proyecto. Luego, puedes utilizar la función `NSTemporaryDirectory()` para obtener una ruta de acceso al directorio temporal en el dispositivo en el que se está ejecutando tu código.

```Swift
import Foundation
let temporaryDir = NSTemporaryDirectory()
```

A continuación, puedes crear un archivo temporal en ese directorio utilizando la función `FileManager.default`. Pasando la ruta de acceso al directorio temporal y un nombre de archivo único como parámetros.

```Swift
let tempFileURL = URL(fileURLWithPath: temporaryDir).appendingPathComponent("miArchivoTemporal.txt")
```

Ahora, puedes escribir o leer información en tu archivo temporal utilizando las funciones de escritura y lectura de archivos en Swift. Una vez que hayas terminado de usar el archivo, asegúrate de eliminarlo utilizando la función `FileManager.default.removeItem(at: tempFileURL)` para liberar espacio en tu dispositivo.

## Profundizando en la creación de archivos temporales

Cuando utilizas la función `NSTemporaryDirectory()` para obtener la ruta de acceso al directorio temporal, debes tener en cuenta que este directorio puede cambiar en diferentes ejecuciones de tu código. Por lo tanto, siempre es importante verificar si el archivo existe antes de intentar escribir o leer información en él.

Además, también puedes establecer una caducidad para tu archivo temporal utilizando la propiedad `isTemporary` en el objeto `URL`. Esto asegurará que el archivo se elimine automáticamente después de un cierto período de tiempo.

## Ver también

Si deseas obtener más información sobre cómo trabajar con archivos en Swift, puedes consultar los siguientes recursos:

- [Documentación oficial de Apple sobre gestión de archivos en Swift](https://developer.apple.com/documentation/foundation/file_management)
- [Artículo en el blog de Ray Wenderlich sobre cómo trabajar con archivos en Swift](https://www.raywenderlich.com/276622-ios-files-and-folders-in-swift)
- [Tutorial de Hacking with Swift sobre cómo crear, escribir y leer archivos en Swift](https://www.hackingwithswift.com/read/12/2/writing-to-disk-nsdata)