---
title:                "Comprobando si existe un directorio"
html_title:           "Swift: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si existe un directorio?

Comprobar si un directorio existe es una tarea común en la programación Swift. Puede ser útil para verificar si un archivo específico está disponible antes de realizar operaciones, como guardar o cargar datos. También puede ser útil para asegurarse de que un directorio de destino esté presente antes de copiar archivos. 

## Cómo hacerlo
```Swift
let fileManager = FileManager.default
var isDirectory: ObjCBool = false

// Comprobación utilizando el método "fileExists" de FileManager.
if fileManager.fileExists(atPath: "/Users/Guillermo/Desktop/NewFolder", isDirectory: &isDirectory) {
    // El directorio existe.
    if isDirectory.boolValue {
        // Es un directorio.
        print("El directorio existe.")
    } else {
        // Es un archivo.
        print("El directorio no existe.")
    }
} else {
    // El directorio no existe.
    print("El directorio no existe.")
}
```

Este código utiliza el método ¨fileExists" de FileManager para determinar si un directorio existe en una ruta de archivo específica. El parámetro "isDirectory" se establece en "true" si el elemento encontrado en la ruta es un directorio, y en "false" si es un archivo. Puedes usar esta información para decidir qué acciones tomar en función del resultado de la comprobación. 

## Deep Dive
Si estás familiarizado con la programación en C, es posible que hayas notado que el método "fileExists" de FileManager es similar a la función "stat" en ese lenguaje. Esto se debe a que en Swift, como en muchos otros lenguajes modernos, gran parte de la funcionalidad de bajo nivel se ha encapsulado en clases y métodos más fáciles de usar para los desarrolladores. Además, al utilizar "objcBool" como tipo de datos para el parámetro "isDirectory", podemos obtener una respuesta más precisa en lugar de simplemente un valor booleano. 

## Ver también
- [Documentación oficial de FileManager en Swift](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial sobre gestión de archivos y directorios en Swift](https://www.raywenderlich.com/863-creating-a-file-manager-in-swift)
- [Artículo sobre el uso de URL para manejar archivos y directorios en Swift](https://www.appcoda.com/swift-url/)