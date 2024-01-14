---
title:    "Swift: Comprobando si existe un directorio"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué

Comprobar si un directorio existe es una tarea común en la programación en Swift. Puede ser útil para asegurarse de que un directorio necesario para nuestro código está presente antes de continuar con la ejecución del programa.

## Cómo hacerlo

Para verificar si un directorio existe, utilizaremos el método `fileExists(atPath:)` del objeto `FileManager`, que nos permite verificar la existencia de un archivo o directorio en una determinada ruta. Veamos un ejemplo de cómo utilizarlo:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/usuario/Proyectos/archivos"

if fileManager.fileExists(atPath: directoryPath) {
    print("El directorio existe")
} else {
    print("El directorio no existe")
}
```

En este ejemplo, primero creamos una instancia de `FileManager` y luego especificamos la ruta del directorio que deseamos verificar. Si el directorio existe, se imprimirá el mensaje "El directorio existe". De lo contrario, se imprimirá "El directorio no existe".

## Profundizando

Aunque el método `fileExists(atPath:)` es útil, puede ser engañoso ya que solo nos indica si un directorio existe en una ruta específica. No nos informa si el directorio está vacío o no. Podemos mejorar nuestra función de comprobación de directorios utilizando el método `contentsOfDirectory(atPath:)` del objeto `FileManager`. Veamos cómo:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/usuario/Proyectos/archivos"

do {
    let contents = try fileManager.contentsOfDirectory(atPath: directoryPath)

    if contents.isEmpty {
        print("El directorio está vacío")
    } else {
        print("El directorio contiene", contents.count, "archivo(s)")
    }
} catch {
    print("No se pudo verificar el directorio:", error.localizedDescription)
}
```

En este ejemplo, utilizamos el método `contentsofDirectory(atPath:)` para obtener una lista de los contenidos del directorio especificado. Luego, verificamos si esta lista está vacía o no para determinar si el directorio está vacío o contiene archivos.

## Ver también

- [Documentación de Apple sobre FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Cómo crear, leer y escribir archivos en Swift](https://medium.com/@anadevsb/how-to-create-read-and-write-files-in-swift-234515605f3e?source=---------3------------------)
- [Cómo manipular rutas de archivos en Swift](https://www.hackingwithswift.com/example-code/file-management/how-to-combine-paths-together)