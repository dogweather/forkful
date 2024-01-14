---
title:                "Swift: Escribir un archivo de texto."
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Swift

Existen varias razones por las cuales podrías querer escribir un archivo de texto en Swift. Por ejemplo, puedes utilizarlo para almacenar datos de una aplicación, para crear un documento o para guardar información que quieres compartir con otros usuarios.

## Cómo escribir un archivo de texto en Swift

Para escribir un archivo de texto en Swift, puedes seguir los siguientes pasos:

1. Crea una instancia de `FileManager`.
2. Utiliza el método `createFile` de `FileManager` para crear un archivo nuevo.
3. Utiliza el método `write` de `String` para escribir el texto que quieres guardar en el archivo.
4. Cierra el archivo utilizando el método `close` de `FileHandle`.

Un ejemplo de código en Swift para escribir un archivo de texto podría ser el siguiente:

```Swift
let fileManager = FileManager()
let fileURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("archivo.txt")
fileManager.createFile(atPath: fileURL.path, contents: nil, attributes: nil)

let texto = "¡Hola mundo!"
let fileHandle = FileHandle(forWritingAtPath: fileURL.path)
fileHandle?.write(texto.data(using: .utf8)!)
fileHandle?.close()

print("Archivo de texto creado con éxito en: \(fileURL.path)")
```

El resultado de este código sería un archivo de texto llamado "archivo.txt" que contiene el texto "¡Hola mundo!" guardado en la ubicación especificada.

## Profundizando en la escritura de archivos de texto en Swift

La clase `FileManager` ofrece varios métodos para crear, leer y escribir archivos. También puedes utilizar la clase `FileHandle` para realizar operaciones más avanzadas en archivos, como mover el cursor de lectura o escritura en una posición específica.

Además, es posible especificar opciones adicionales al crear un archivo, como especificar un codificador para el texto que se va a guardar. Puedes consultar la documentación oficial de Apple para obtener más información sobre estas opciones y métodos.

## Ver también

- [Documentación oficial de Apple sobre `FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- [Documentación oficial de Apple sobre `FileHandle`](https://developer.apple.com/documentation/foundation/filehandle)
- [Guía de Swift de Udemy sobre escritura de archivos de texto](https://www.udemy.com/course/swift-escreviendo-un-archivo-de-texto-en-swift-ios/?LSNPUBID=qB8hOSNAg0Y&%3Butm_source=aff-campaign&%3Butm_medium=udemyads&%3Butm_term=Homepage&%3Butm_campaign=Web+Company+Homepage+LAN&%3Butm_content=Textlink)