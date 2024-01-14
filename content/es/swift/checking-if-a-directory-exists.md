---
title:    "Swift: Comprobando si existe un directorio"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, a menudo necesitamos verificar si un directorio existe antes de realizar cualquier operación en él. Esto puede ser útil para asegurarse de que se tenga acceso al directorio correcto o para evitar errores en el código.

## Cómo hacerlo
Para verificar si un directorio existe en Swift, podemos utilizar la clase `FileManager` y su método `fileExists(atPath:)`. Este método toma como argumento la ruta al directorio que queremos verificar y devuelve un booleano que indica si el directorio existe o no. Aquí hay un ejemplo de cómo podemos implementar esto en nuestro código:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/username/folder/"

if fileManager.fileExists(atPath: directoryPath) {
    print("El directorio existe")
} else {
    print("El directorio no existe")
}
```

En este ejemplo, estamos utilizando la ruta al directorio "folder" en el directorio principal del usuario y verificando si existe o no. Luego imprimimos un mensaje apropiado en consecuencia.

## Profundizando
Si queremos llevar nuestra verificación de directorio un paso más allá, podemos utilizar el método `contentsOfDirectory(atPath:)` de la clase `FileManager`. Este método devuelve una matriz con el contenido del directorio especificado, incluyendo tanto archivos como otros subdirectorios. Podemos utilizar esta información para realizar acciones adicionales en nuestro código, como enumerar los archivos en el directorio o verificar si existen subdirectorios.

Es importante tener en cuenta que, aunque verifiquemos que un directorio existe, aún podemos enfrentar errores si tratamos de realizar operaciones en él sin tener los permisos adecuados. Por lo tanto, siempre es importante verificar los permisos de acceso antes de realizar cualquier acción en un directorio.

## Ver también
- Documentación oficial de Apple para `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Ejemplo práctico de verificación de directorio en Swift: https://dev.to/ritikraghuvanshi08/check-if-a-directory-exists-in-swift-4k77