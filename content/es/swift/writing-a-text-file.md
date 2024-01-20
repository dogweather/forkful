---
title:                "Escritura de un archivo de texto"
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Escribir archivos de texto en Swift es guardar texto plano en un archivo en tu disco. Los programadores lo hacen para persistencia de datos, como configuraciones, o para compartir información entre procesos o usuarios.

## Cómo hacerlo:
Para escribir en un archivo de texto, utilizamos la clase `FileManager` y `String` para manejar la escritura. Aquí hay un ejemplo:

```Swift
import Foundation

let texto = "¡Hola, mundo!"
let ubicacion = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
let archivoUrl = ubicacion.appendingPathComponent("saludo.txt")

do {
    try texto.write(to: archivoUrl, atomically: true, encoding: .utf8)
    print("Archivo guardado en: \(archivoUrl)")
} catch {
    print("Ocurrió un error al guardar el archivo: \(error)")
}
```

Ejecuta el código, y crea un archivo `saludo.txt` con el texto "¡Hola, mundo!" en tu directorio de documentos.

## Profundizando:
- Histórico: Antes de Swift, Objective-C era el lenguaje principal para desarrollo en iOS, y el manejo de archivos era más complicado.
- Alternativas: Puedes usar `FileHandle` o `OutputStream` para más control o para escribir en archivos grandes de manera eficiente.
- Detalles de Implementación: Usa `atomically` para seguridad, escribe primero en un archivo temporal y luego lo renombra, evitando así corrupción de datos.

## Ver Además:
- Documentación de Apple sobre FileManager: [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- Tutorial sobre Swift File I/O: [Ray Wenderlich](https://www.raywenderlich.com/1973-file-i-o-tutorial-for-ios-how-to-read-write-files-in-swift)
- Guía para `FileHandle`: [NSFileHandle](https://developer.apple.com/documentation/foundation/filehandle)