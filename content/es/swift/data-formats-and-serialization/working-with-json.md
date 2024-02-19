---
aliases:
- /es/swift/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:01.079897-07:00
description: "Trabajar con JSON en Swift implica lidiar con un formato de datos ligero\
  \ para el intercambio de datos. Los programadores utilizan JSON para transmitir\u2026"
lastmod: 2024-02-18 23:09:10.381570
model: gpt-4-0125-preview
summary: "Trabajar con JSON en Swift implica lidiar con un formato de datos ligero\
  \ para el intercambio de datos. Los programadores utilizan JSON para transmitir\u2026"
title: Trabajando con JSON
---

{{< edit_this_page >}}

## Qué y Por Qué?

Trabajar con JSON en Swift implica lidiar con un formato de datos ligero para el intercambio de datos. Los programadores utilizan JSON para transmitir datos entre un servidor y una aplicación web porque es legible y fácil de analizar tanto para humanos como para máquinas.

## Cómo hacerlo:

Swift facilita el análisis de JSON con el protocolo `Codable`. Así es cómo puedes decodificar JSON a un objeto Swift:

```Swift
import Foundation

// Definir un modelo que se ajuste a Codable
struct User: Codable {
    var name: String
    var age: Int
}

// Cadena de JSON
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// Convertir la cadena JSON a Data
if let jsonData = jsonString.data(using: .utf8) {
    // Decodificar los datos JSON a un objeto User
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Nombre: \(user.name), Edad: \(user.age)")
    } catch {
        print("Error decodificando JSON: \(error)")
    }
}
```

Salida de muestra:
```
Nombre: John Doe, Edad: 30
```

## Análisis Profundo

JSON (JavaScript Object Notation) ha sido ampliamente adoptado desde principios de los años 2000, después de que Douglas Crockford lo especificara. Reemplazó a XML en muchos casos de uso debido a su sintaxis más simple y mejor rendimiento. Aunque `Codable` de Swift es la opción predilecta para JSON, existen alternativas como `JSONSerialization` para cuando se trata de tipos que no cumplen con Codable. Bajo el capó, `Codable` abstrae el análisis de bajo nivel y hace que la serialización/deserialización sea fluida.

## Ver También

- Explora más sobre JSON y Swift en el blog oficial de Swift: [Swift.org](https://swift.org/blog/)
- Consulta la documentación de `Codable`: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- Para estructuras JSON complejas, considera bibliotecas de terceros como SwiftyJSON disponibles en [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
