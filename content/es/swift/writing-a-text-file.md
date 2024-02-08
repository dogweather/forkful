---
title:                "Escribiendo un archivo de texto"
aliases:
- es/swift/writing-a-text-file.md
date:                  2024-02-03T19:29:21.331835-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo un archivo de texto"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir un archivo de texto en Swift te permite almacenar de manera persistente datos de cadena en el sistema de archivos, lo cual es esencial para tareas como guardar configuraciones, datos de usuario o registros. Los programadores a menudo hacen esto para mantener datos entre lanzamientos de la app, compartir datos entre diferentes partes de una aplicación o exportar datos para ser usados por otros programas.

## Cómo hacerlo:

### Usando la Biblioteca Estándar de Swift

La biblioteca estándar de Swift incluye todas las herramientas necesarias para escribir archivos de texto. Aquí tienes un enfoque básico:

```swift
import Foundation

let content = "¡Hola, lectores de Wired! Aprender Swift es divertido."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/ejemplo.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("Archivo escrito con éxito")
} catch let error as NSError {
    print("Error al escribir en URL: \(fileName), Error: " + error.localizedDescription)
}
```

Este fragmento de código escribe una cadena en un archivo llamado `ejemplo.txt` en el directorio de documentos. Maneja errores potenciales usando el manejo de errores do-try-catch de Swift.

### Usando FileManager para Más Control

Para más control sobre los atributos del archivo o para verificar si el archivo ya existe, se puede utilizar `FileManager`:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("ejemplo.txt")
    let content = "Explorar Swift para la gestión de archivos es iluminador."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("El archivo ya existe")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("Archivo creado y escrito con éxito")
        } catch {
            print("Error al escribir el archivo: \(error)")
        }
    }
}
```

### Usando Bibliotecas de Terceros

Una biblioteca de terceros popular para operaciones de sistema de archivos en Swift es `Files` de John Sundell:

Primero, agrega Files a tu proyecto, usualmente vía Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TuNombreDePaquete",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "TuNombreDeObjetivo",
            dependencies: ["Files"]),
    ]
)
```

Luego, úsalo para escribir en un archivo:

```swift
import Files

do {
    let file = try File(path: "/ruta/a/tu/directorio/ejemplo.txt")
    try file.write(string: "Swift y la biblioteca Files hacen una combinación poderosa.")
    print("Archivo escrito con éxito usando la biblioteca Files.")
} catch {
    print("Ha ocurrido un error: \(error)")
}
```

Con la biblioteca `Files`, manejar archivos se vuelve más sencillo, permitiéndote enfocarte en la lógica de negocio de tu aplicación en vez de en los detalles técnicos de la gestión de archivos.
