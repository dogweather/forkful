---
title:                "Swift: Creando un archivo temporal"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una forma común de almacenar información o datos de manera temporal en una aplicación Swift. Esto es especialmente útil cuando se necesita procesar o manipular cierta información antes de guardarla permanentemente o compartirla con otros usuarios. Con las herramientas adecuadas, es muy sencillo y útil crear un archivo temporal en Swift.

## Cómo hacerlo

Para crear un archivo temporal en Swift, necesitamos primero importar la biblioteca Foundation, ya que contiene las herramientas necesarias para manipular archivos. Luego se puede utilizar la función `NSTemporaryDirectory()` para obtener la ruta del directorio temporal del sistema y la función `URL(fileURLWithPath:)` para crear una URL utilizando la ruta del directorio temporal.

```Swift
import Foundation

let temporaryDirectory = NSTemporaryDirectory()
let temporaryURL = URL(fileURLWithPath: temporaryDirectory)
print(temporaryURL)
```

La salida de este código será una URL que apunta al directorio temporal del sistema:

```
/var/folders/9l/j8wqmyf97z9gxrq2rn7g14880000gn/T/
```

Una vez que tenemos la URL del directorio temporal, podemos utilizar la función `URL.appendingPathComponent(_:)` para agregar un nombre de archivo al final de la URL y crear así un archivo temporal con ese nombre. Veamos un ejemplo:

```Swift
import Foundation

let temporaryDirectory = NSTemporaryDirectory()
let temporaryURL = URL(fileURLWithPath: temporaryDirectory)
let temporaryFileURL = temporaryURL.appendingPathComponent("file.txt")
print(temporaryFileURL)
```

La salida de este código será una URL que apunta al archivo `file.txt` dentro del directorio temporal del sistema:

```
/var/folders/9l/j8wqmyf97z9gxrq2rn7g14880000gn/T/file.txt
```

Ahora que tenemos la URL del archivo temporal, podemos utilizarla para escribir o leer información en él utilizando las funciones de la clase `Data` de Foundation. Una vez que hayamos terminado de usar el archivo temporal, podemos eliminarlo utilizando la función `FileManager.default.removeItem(atPath:)`.

## Profundizando

Crear un archivo temporal en Swift puede parecer una tarea simple, pero entender cómo funciona y cómo utilizarlo de manera adecuada puede ser de gran ayuda en el desarrollo de aplicaciones. Conocer las funciones y clases adecuadas de la biblioteca Foundation nos permite manipular archivos de manera sencilla y eficiente.

Una de las principales ventajas de utilizar un archivo temporal es que no afecta al funcionamiento del sistema, ya que se elimina automáticamente cuando el programa termina de usarlo. Además, también es útil cuando se trata de datos sensibles, ya que no es necesario mantener la información almacenada en el sistema después de su uso.

## Ver también

- [Documentación de Apple sobre la clase FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Documentación de Apple sobre la clase Data](https://developer.apple.com/documentation/foundation/data)
- [Tutorial de Hacking with Swift sobre la creación de archivos temporales](https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-file-on-disk-using-nsfilemanager)