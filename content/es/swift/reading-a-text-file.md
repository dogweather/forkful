---
title:    "Swift: Leyendo un archivo de texto"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Leer un archivo de texto puede ser una habilidad útil para los programadores. Con este conocimiento, podrás manipular y analizar datos almacenados en formato de texto sin dificultad. Aprender a leer archivos de texto en Swift puede abrir muchas posibilidades y mejorar tus habilidades de programación.

## Cómo hacerlo

Para leer un archivo de texto en Swift, primero necesitas especificar la ubicación del archivo que quieres leer. Esto se puede hacer mediante el uso del objeto `URL` y especificando la ruta del archivo.

```Swift
let fileURL = URL(fileURLWithPath: "ruta/al/archivo.txt")
```
Una vez que tengas la ubicación del archivo, puedes usar la clase `String` para leer el contenido del archivo en una sola cadena de texto. 

```Swift
if let fileContents = try? String(contentsOf: fileURL) {
    print(fileContents)
}
```
Este código leerá el contenido del archivo en la variable `fileContents` y lo imprimirá en la consola.

## Profundizando

Hay diferentes formas de leer un archivo de texto en Swift dependiendo de tus necesidades. Puedes usar la clase `FileHandle` si necesitas leer el contenido del archivo en bloques en lugar de una sola cadena de texto. También puedes especificar el tipo de codificación del archivo, como UTF-8 o ASCII.

Además de leer, también puedes escribir en archivos de texto utilizando el método `write(to:atomically:encoding)` de la clase `String`. Este método te permite escribir texto en un archivo en una ubicación específica.

## Ver también

- [Cómo escribir y leer archivos en Swift](https://www.ioscreator.com/tutorials/read-and-write-files-swift)
- [Documentación oficial de Apple sobre el manejo de archivos en Swift](https://developer.apple.com/documentation/foundation/file_management)
- [Tutorial de Hacking with Swift sobre la lectura de archivos de texto](https://www.hackingwithswift.com/example-code/strings/how-to-read-a-text-file-into-a-string)