---
title:                "Swift: Leyendo un archivo de texto."
simple_title:         "Leyendo un archivo de texto."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Los archivos de texto son una forma común de almacenar información en un dispositivo. Al aprender a leer y manipular estos archivos, los programadores pueden crear aplicaciones más versátiles y eficientes.

## Cómo hacerlo

Para leer un archivo de texto en Swift, primero debes crear una instancia de `FileManager` y obtener el `URL` del archivo que deseas leer. Luego, puedes usar el método `contents(atPath:)` del `FileManager` para leer el contenido del archivo en forma de `Data`.

```
let fileManager = FileManager.default
let url = URL(fileURLWithPath: "ruta_del_archivo/texto.txt")

if let archivo = fileManager.contents(atPath: url.path) {
    // Hacer algo con el contenido del archivo
} else {
    // Manejar el error si el archivo no se puede leer
}
```

Otra forma de leer un archivo de texto es utilizando `String` y su método `init(contentsOfFile:)`. Este método convertirá automáticamente el contenido del archivo en forma de `String`.

```
if let texto = String(contentsOfFile: "ruta_del_archivo/texto.txt") {
    // Hacer algo con el texto del archivo
} else {
    // Manejar el error si el archivo no se puede leer
}
```

## Profundizando

Hay muchas formas de leer y manipular archivos de texto en Swift, incluyendo la lectura línea por línea utilizando el método `components(separatedBy:)`, el uso de `Codable` para leer y escribir en formato JSON, y la lectura de archivos CSV utilizando librerías externas.

Además, es importante recordar cerrar el archivo después de leerlo para evitar posibles errores o conflictos. Puedes hacer esto utilizando el método `close()` después de terminar de leer los datos.

```
archivo.close()
```

## Ver también

- [Apple Developer Documentation: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Developer Documentation: String](https://developer.apple.com/documentation/swift/string)
- [Swift.org: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)