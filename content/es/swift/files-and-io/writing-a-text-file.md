---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:21.331835-07:00
description: "C\xF3mo hacerlo: La biblioteca est\xE1ndar de Swift incluye todas las\
  \ herramientas necesarias para escribir archivos de texto. Aqu\xED tienes un enfoque\
  \ b\xE1sico."
lastmod: '2024-03-13T22:44:59.434824-06:00'
model: gpt-4-0125-preview
summary: "La biblioteca est\xE1ndar de Swift incluye todas las herramientas necesarias\
  \ para escribir archivos de texto."
title: Escribiendo un archivo de texto
weight: 24
---

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
