---
title:                "Swift: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por Qué
Crear un archivo temporal es una técnica útil en programación para almacenar datos de forma temporal en una aplicación.

## Cómo
Se puede crear un archivo temporal en Swift usando la clase FileManager y su método `temporaryDirectory`. A continuación se muestra un ejemplo de código que crea un archivo temporal con un nombre único y escribe en él una cadena de texto:

```Swift
let fileManager = FileManager.default

// Obtener ruta del directorio temporal
let tempDirectoryURL = fileManager.temporaryDirectory

// Crear un nombre único para el archivo temporal
let tempFileName = ProcessInfo.processInfo.globallyUniqueString

// Combinar la ruta del directorio temporal con el nombre del archivo temporal
let tempFileURL = tempDirectoryURL.appendingPathComponent(tempFileName)

do {
    // Crear el archivo temporal
    try "Este es un texto temporal".write(to: tempFileURL, atomically: true, encoding: .utf8)
    
    // Imprimir la ruta del archivo temporal
    print("Archivo temporal creado en: \(tempFileURL)")
    
    // Obtener el contenido del archivo temporal
    let tempFileContent = try String(contentsOf: tempFileURL, encoding: .utf8)
    
    // Imprimir el contenido del archivo temporal
    print("Contenido del archivo temporal: \(tempFileContent)")
} catch {
    // Manejar errores aquí
}
```

El código anterior crea un archivo temporal en el directorio temporal predeterminado del sistema y escribe en él una cadena de texto. Luego, lee el contenido del archivo temporal y lo imprime en la consola.

## Deep Dive
Crear un archivo temporal en Swift es útil cuando se necesita almacenar datos de forma temporal en una aplicación. Por ejemplo, puede ser útil para almacenar datos descargados de una API o para generar archivos que solo se utilizarán temporalmente. 

Hay varias opciones para personalizar la creación de un archivo temporal. Por ejemplo, se puede especificar un prefijo o sufijo para el nombre del archivo temporal utilizando los métodos `withPrefix()` y `withSuffix()` de FileManager. También se pueden establecer atributos adicionales, como la fecha de modificación o permisos, utilizando el método `createFile()`. 

Además, es importante tener en cuenta que los archivos temporales se eliminan automáticamente cuando se cierra la aplicación o el sistema se reinicia, por lo que es importante copiar o mover el contenido de un archivo temporal a un archivo permanente si se desea conservarlo.

## Ver También
- [Documentación de FileManager en Swift](https://developer.apple.com/documentation/foundation/filemanager)
- [Artículo sobre la creación de archivos y directorios en Swift](https://www.hackingwithswift.com/read/9/3/creating-directory-and-files)
- [Tutorial sobre el uso de archivos temporales en Swift](https://learnappmaking.com/temporary-files-swift-ios/)