---
title:                "Swift: Comprobando si existe un directorio"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe?

A menudo, al escribir código en Swift, es necesario realizar diferentes acciones en un directorio específico. Antes de realizar cualquier acción, es importante asegurarse de que el directorio exista en primer lugar. De lo contrario, podría haber errores en la ejecución del código.

## Cómo hacerlo: 

Una forma de comprobar si un directorio existe en Swift es utilizando la función `fileExists(atPath:)` de la clase `FileManager`. Esta función devuelve un booleano que indica si el directorio existe o no. Podemos utilizar una estructura `if` para tomar diferentes acciones dependiendo del resultado de esta función.

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/username/Desktop/Folder"

if fileManager.fileExists(atPath: directoryPath) {
    // El directorio existe, realizamos acciones aquí
    print("El directorio existe en la ruta especificada.")
} else {
    // El directorio no existe, realizamos acciones aquí
    print("El directorio no existe en la ruta especificada.")
}
```
Como se puede ver en el código de ejemplo, utilizamos la función `fileExists(atPath:)` para comprobar si el directorio en la ruta especificada existe. Si es así, podemos realizar las acciones necesarias, de lo contrario, podemos mostrar un mensaje de error o crear el directorio si es necesario.

## Profundizando: 

Si queremos profundizar aún más en el tema de comprobar si un directorio existe en Swift, también podemos utilizar la función `fileExists(atPath:)` para comprobar si un archivo específico existe en un directorio. Simplemente tendríamos que especificar el nombre del archivo en la ruta de la función en lugar de la ruta del directorio.

```Swift
let fileManager = FileManager.default
let filePath = "/Users/username/Desktop/Folder/file.txt"

if fileManager.fileExists(atPath: filePath) {
    // El archivo existe, realizamos acciones aquí
    print("El archivo existe en el directorio.")
} else {
    // El archivo no existe, realizamos acciones aquí
    print("El archivo no existe en el directorio.")
}
```

También podemos utilizar la función `fileExists(atPath:)` para comprobar si un directorio específico está vacío o no. Si el directorio está vacío, podemos borrarlo o realizar otras acciones, y si contiene archivos, podemos realizar otras acciones diferentes.

## Ver también: 

Para obtener más información sobre cómo trabajar con directorios y archivos en Swift, puedes consultar la documentación oficial de Apple en [FileManager](https://developer.apple.com/documentation/foundation/filemanager) y [URL](https://developer.apple.com/documentation/foundation/url).

También puedes explorar otras funciones útiles para trabajar con directorios y archivos, como `createDirectory(atPath:withIntermediateDirectories:attributes:)` para crear un directorio y sus subdirectorios, `files(atPath:)` para obtener una lista de archivos en un directorio específico, y `removeItem(atPath:)` para borrar un archivo o directorio. ¡Espero que esta información te sea útil en tu desarrollo con Swift!