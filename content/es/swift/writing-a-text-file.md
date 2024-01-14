---
title:                "Swift: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Swift

¿Por qué alguien se involucraría en la tarea de escribir un archivo de texto en Swift? Hay varias razones por las que podría ser útil, incluyendo la creación de registros de datos, la generación de informes o incluso la creación de contenido para una aplicación. A continuación, exploraremos cómo hacerlo en Swift y profundizaremos en algunos detalles sobre la escritura de archivos de texto.

## Cómo hacerlo

Para empezar, necesitamos importar la biblioteca Foundation de Swift. Luego, podemos utilizar el método "write" para escribir en un archivo de texto específico. Aquí hay un ejemplo de código que escribe una cadena en un nuevo archivo de texto llamado "miarchivo.txt":

```Swift 
import Foundation

let stringToWrite = "Hola, mundo!"

do {
    try stringToWrite.write(toFile: "miarchivo.txt", atomically: true, encoding: String.Encoding.utf8)
} catch {
    print("¡Error al escribir en el archivo!")
}
```

Al ejecutar este código, debería ver un nuevo archivo de texto llamado "miarchivo.txt" en su directorio de trabajo actual con el contenido "Hola, mundo!". También puede especificar una ruta de archivo completa en lugar de solo un nombre de archivo en la línea "toFile" para escribir en un directorio específico.

## Profundizando

Cuando usamos el método "write" para escribir en un archivo de texto, en realidad estamos utilizando la estructura FileManager detrás de escena. Esta estructura contiene una variedad de otros métodos para trabajar con archivos, incluyendo "exists" para verificar si un archivo ya existe, "removeItem" para eliminar un archivo y "createFile" para crear uno nuevo. También hay opciones para especificar el formato de codificación del archivo y si se desea o no que se escriban datos sincrónicamente.

Si desea escribir en un archivo de texto sin sobrescribir su contenido actual, puede agregar el modificador "append" al método "write" en lugar de "atomically".

## Vea también

- [Documentación de Apple sobre escritura de archivos de texto en Swift](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial sobre escritura de archivos de texto en Swift](https://www.hackingwithswift.com/read/15/overview)
- [Ejemplos de escritura de archivos de texto en Swift](https://www.swifttutorial.com/write-text-file/)