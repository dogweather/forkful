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

¿Qué y por qué?
Comprobar si un directorio existe es una operación común en la programación Swift. Esencialmente, se trata de verificar si hay una carpeta en una ubicación específica en el sistema. Los programadores suelen hacer esto para garantizar que su código funcione correctamente y manejar posibles errores si el directorio no existe.

Cómo hacerlo:
Para verificar si un directorio existe en Swift, podemos utilizar la función `FileManager.default.fileExists()` y pasar como argumento la ruta del directorio que queremos comprobar. Por ejemplo, si queremos comprobar si existe un directorio llamado "Documentos" en el directorio de inicio del usuario, podemos hacer lo siguiente:

```Swift
if FileManager.default.fileExists(atPath: "/Users/Usuario/Documentos") {
    print("El directorio existe.")
} else {
    print("El directorio no existe.")
}
```

Profundizando:
En el pasado, para comprobar si un directorio existía, era necesario utilizar funciones en C y Objective-C. Sin embargo, con la introducción del framework `FileManager` en Swift, ahora es mucho más sencillo. Además de `fileExists()`, también podemos utilizar la función `fileExists(atPath:)` para comprobar la existencia de un archivo específico en lugar de un directorio.

Véase también:
Para obtener más información sobre la comprobación de la existencia de directorios y archivos en Swift, consulta la documentación oficial de Apple sobre `FileManager`. También puedes encontrar más detalles sobre la gestión de directorios en Swift en el blog de Swift by Sundell.