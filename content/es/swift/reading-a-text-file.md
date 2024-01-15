---
title:                "Leyendo un archivo de texto"
html_title:           "Swift: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué
Si estás interesado en crear aplicaciones para iOS o macOS, es importante que sepas cómo leer un archivo de texto en Swift. Esto te permitirá acceder y procesar información almacenada en formatos de texto, como CSV o JSON, y utilizarla en tus aplicaciones.

## Cómo hacerlo
Para leer un archivo de texto en Swift, primero debes asegurarte de tener el archivo en tu proyecto y que esté habilitado para ser leído por tu aplicación. Luego, puedes utilizar la clase `FileManager` para obtener la URL del archivo y la clase `String` para leer su contenido. Aquí tienes un ejemplo de cómo hacerlo:

```Swift
// Obtener la URL del archivo
let fileManager = FileManager.default
if let fileURL = fileManager.url(for: "miArchivo.txt", in: .documentDirectory) {
    // Leer el contenido del archivo como un String
    do {
        let fileContents = try String(contentsOf: fileURL)
        print(fileContents)
    } catch {
        // Manejar errores
        print("No se pudo leer el archivo.")
    }
}
```

El código anterior obtiene la URL del archivo `miArchivo.txt` ubicado en el directorio de documentos de tu aplicación y luego utiliza la función `String(contentsOf:)` para leer su contenido y almacenarlo en la constante `fileContents`.

## Profundizando
Al leer un archivo de texto en Swift, es importante tener en cuenta que el contenido del archivo se leerá como un String, lo que significa que se trata de una secuencia de caracteres. Por lo tanto, si necesitas procesar la información como números o valores booleanos, deberás convertir manualmente el String en el tipo de dato correspondiente. Por ejemplo, si tienes un archivo CSV con números separados por comas, deberás utilizar la función `components(separatedBy:)` para separar los valores y luego convertirlos a números utilizando la clase `NumberFormatter`.

Además, es importante manejar adecuadamente los posibles errores al leer un archivo de texto. Asegúrate de utilizar un `do-catch` block para capturar cualquier error y manejarlo adecuadamente en tu código.

## Ver también
- [Documentación oficial de Apple sobre la clase FileManager en Swift](https://developer.apple.com/documentation/foundation/filemanager)
- [Documentación oficial de Apple sobre la clase String en Swift](https://developer.apple.com/documentation/swift/string)
- [Tutorial de Ray Wenderlich sobre cómo leer y escribir archivos en Swift](https://www.raywenderlich.com/1006565-how-to-work-with-files-and-directories-in-swift-part-1)