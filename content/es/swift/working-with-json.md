---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

Trabajar con JSON significa manipular el formato de intercambio de datos JavaScript Object Notation. Los programadores lo hacen porque es ligero, fácil de leer para humanos y esencial para la comunicación en aplicaciones web y móviles.

## Cómo hacerlo:

```Swift
import Foundation

// Definir un struct para mapear los datos JSON
struct Usuario: Codable {
    var nombre: String
    var edad: Int
}

// Simular un JSON desde un string para el ejemplo
let jsonString = "{\"nombre\": \"Juan\", \"edad\": 30}"

// Convertir el string JSON a datos
if let jsonData = jsonString.data(using: .utf8) {
    do {
        // Decodificar el JSON a nuestro struct
        let usuario = try JSONDecoder().decode(Usuario.self, from: jsonData)
        print("Nombre: \(usuario.nombre), Edad: \(usuario.edad)")
    } catch {
        print("Error al decodificar: \(error)")
    }
}
```

Salida de ejemplo:
```
Nombre: Juan, Edad: 30
```

## Profundización:

1. **Contexto histórico**: JSON se originó en los primeros años 2000 como una manera más sencilla de usar alternativa a XML en AJAX.
2. **Alternativas**: XML es una alternativa más antigua, pero más compleja. Protobuf de Google y BSON son consideradas para sistemas que necesitan serialización binaria.
3. **Detalles de implementación**: Swift utiliza el protocolo `Codable` para codificar y decodificar. La key strategy de `JSONDecoder` y `JSONEncoder` puede ser usada para trabajar con diferentes estilos de claves JSON.

## Ver También:

- Documentación oficial de Swift sobre JSON: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/jsonencoder)
- RFC 7159 - The JavaScript Object Notation (JSON) Data Interchange Format: [RFC 7159](https://tools.ietf.org/html/rfc7159)
- Tutorial de Ray Wenderlich sobre JSON y Swift: [Ray Wenderlich](https://www.raywenderlich.com/3418439-encoding-and-decoding-in-swift)