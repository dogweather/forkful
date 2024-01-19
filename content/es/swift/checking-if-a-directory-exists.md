---
title:                "Verificando si un directorio existe"
html_title:           "Javascript: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Verificar si un directorio existe es un proceso en el cual, con código, determinamos la existencia de un directorio específico en el sistema de archivos. Los programadores lo hacen para evitar errores durante la operación de lectura o escritura de archivos.

## ¿Cómo se hace?

En Swift, puedes comprobar si un directorio existe con funciones de FileManager. Aquí tienes un ejemplo:

```Swift
import Foundation

let fileManager = FileManager.default
let directorio = "/ruta/tu_directorio" 

if fileManager.fileExists(atPath: directorio) {
    print("El directorio existe.")
} else {
    print("El directorio no existe.")
}
```
En este código, primero importamos el módulo Foundation y luego inicializamos una instancia de `FileManager`. Luego, verificamos si un directorio con la ruta dada existe o no.

## Un Vistazo Más Profundo

Históricamente, diversos lenguajes de programación han implementado formas de verificación de la existencia de directorios, debido a que la operación de archivos es un componente crucial en programación. En Swift, el módulo Foundation proporciona la clase `FileManager` para gestionar los archivos y directorios.

Además de `fileExists(atPath:)`, Swift ofrece `attributesOfItem(atPath:)`, que puede proporcionar más información detallada, como fecha de creación y modificación, del directorio.

```Swift
try? fileManager.attributesOfItem(atPath: directorio)
```
Esta función devolverá `nil` si el directorio no existe, lo cual también te dice que el directorio no existe. Si el directorio existe, te dará un diccionario con sus atributos.

## Ver También

- "Trabajar con rutas de archivos y directorios": https://developer.apple.com/documentation/foundation/filemanager
- "Verificar si un archivo o directorio existe": https://stackoverflow.com/questions/30056471/check-if-a-file-exists-in-swift

En estos enlaces puedes encontrar información más detallada sobre cómo trabajar con directorios y archivos en Swift. Las referencias te ayudarán a entender mejor y aplicar de manera efectiva estas operaciones.