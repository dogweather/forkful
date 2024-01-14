---
title:                "Swift: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, es común encontrarse con situaciones en las que necesitamos saber si un directorio existe antes de realizar alguna acción. Aprender a comprobar si un directorio existe es una habilidad esencial para cualquier programador de Swift.

## Cómo hacerlo

Comprobar si un directorio existe en Swift es bastante sencillo. Utilizando la clase `FileManager`, podemos acceder a una variedad de métodos que nos permiten interactuar con los directorios. En este caso, utilizaremos el método `fileExists(atPath:)` para determinar si un directorio existe o no. Aquí hay un ejemplo de código que muestra cómo se puede hacer esto:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/username/Documents/"
if fileManager.fileExists(atPath: directoryPath) {
    print("¡El directorio existe!")
} else {
    print("El directorio no existe.")
}
```

La salida de este pequeño programa dependerá del valor que se asigne a la variable `directoryPath`. Si se asigna una ruta válida que apunta a un directorio existente, se imprimirá "¡El directorio existe!", de lo contrario, se imprimirá "El directorio no existe".

## Profundizando

Mientras que el método `fileExists(atPath:)` es el más comúnmente utilizado para comprobar si un directorio existe, también existen otros métodos que pueden ser útiles en ciertas situaciones. Por ejemplo, si necesitamos comprobar si un directorio es de solo lectura, podemos utilizar el método `isReadableFile(atPath:)`. También existe el método `isDeletableFile(atPath:)` para verificar si un directorio se puede eliminar.

Además, es importante tener en cuenta que estos métodos pueden arrojar errores en ciertas situaciones, por lo que se recomienda envolver el código en un bloque `do-catch` para manejar cualquier posible error.

## Ver también

- Documentación oficial de Apple sobre la clase `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Tutorial detallado sobre cómo comprobar si un directorio existe en Swift: https://www.raywenderlich.com/24595/files-tutorial-for-ios-how-to-use-nsfilemanager
- Ejemplos de código útiles para trabajar con directorios en Swift: https://appventure.me/guides/filemanager.html