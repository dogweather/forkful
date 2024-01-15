---
title:                "Escribir un archivo de texto"
html_title:           "Swift: Escribir un archivo de texto"
simple_title:         "Escribir un archivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una tarea común en el desarrollo de aplicaciones, ya que permite almacenar y acceder fácilmente a datos importantes de una manera estructurada. También es una forma eficiente de compartir información entre diferentes programas.

## Cómo hacerlo

Para escribir un archivo de texto en Swift, primero debes crear una instancia de la clase FileManager para manejar el proceso de escritura. Luego, puedes utilizar la función "createFile" para crear un nuevo archivo en el directorio especificado. Una vez que tienes el archivo creado, puedes utilizar la función "write" para escribir datos en formato de cadena en él.

````Swift
// Creamos una instancia de FileManager
let fileManager = FileManager.default
// Definimos el nombre y la ubicación del archivo
let fileName = "mi_archivo.txt"
let filePath = fileManager.currentDirectoryPath + "/" + fileName
// Creamos el archivo en el directorio actual
fileManager.createFile(atPath: filePath, contents: nil, attributes: nil)
// Escribimos en el archivo
let texto = "Este es un ejemplo de texto que se escribirá en el archivo"
try texto.write(toFile: filePath, atomically: true, encoding: .utf8)
````

Una vez que se ha ejecutado el código anterior, se creará un archivo llamado "mi_archivo.txt" en el directorio actual y se escribirá el contenido especificado en él.

## Profundizando

Al escribir un archivo de texto en Swift, es importante tener en cuenta algunas consideraciones clave. Primero, siempre se debe asegurar de cerrar el archivo después de su uso utilizando la función "closeFile". También se recomienda utilizar la sintaxis "try-catch" al escribir en un archivo para manejar posibles errores. Además, se pueden agregar atributos de formato específicos al crear el archivo utilizando el parámetro "attributes" en la función "createFile".

## Ver también

- [Documentación de FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Guía de Swift para la gestión de errores](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Ejemplo de escritura de archivo en Swift](https://www.tutorialspoint.com/how-to-write-a-file-in-swift-programming-language)