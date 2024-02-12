---
title:                "Escribiendo en el error estándar"
aliases: - /es/swift/writing-to-standard-error.md
date:                  2024-02-03T19:34:44.309535-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo en el error estándar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir en el error estándar (stderr) se trata de dirigir los mensajes de error o de diagnóstico de tu programa a un flujo separado, distinto del salida estándar (stdout). Esto es crucial para depurar y registrar errores sin saturar la salida estándar, facilitando tanto la comprensión de los desarrolladores como de los usuarios sobre el estado y los problemas del programa.

## Cómo hacerlo:

En Swift, escribir en el error estándar se puede hacer usando la clase `FileHandle` para un acceso directo al stderr. Aquí hay un ejemplo simple:

```swift
import Foundation

// Definir un mensaje
let errorMessage = "Ocurrió un error.\n"

// Convertir el mensaje a datos
if let data = errorMessage.data(using: .utf8) {
    // Escribir el mensaje de error en stderr
    FileHandle.standardError.write(data)
}
```

Salida a stderr (típicamente vista en una consola o terminal):
```
Ocurrió un error.
```

Para registros más complejos o cuando se trabaja con bibliotecas externas, se podría considerar el uso de una biblioteca de terceros como **SwiftLog**. Aunque **SwiftLog** no escribe en stderr directamente de manera predeterminada, puedes implementar un backend de registro personalizado para lograrlo. He aquí un ejemplo simplificado de cómo definir un manejador de registro personalizado que escribe en stderr:

Primero, añade **SwiftLog** a las dependencias de tu proyecto en `Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "TuNombreDePaquete",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "TuNombreDeDestino",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Luego, implementa un manejador de registro personalizado que escribe en stderr:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// Uso
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.tuapp")

logger.error("Este es un mensaje de error")
```

Salida a stderr:
```
Este es un mensaje de error
```

Este manejador personalizado te permite dirigir tus mensajes de error de SwiftLog directamente al error estándar, integrándose de forma transparente con otros mensajes de registro que tu aplicación pueda generar.
