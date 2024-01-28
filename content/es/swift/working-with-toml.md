---
title:                "Trabajando con TOML"
date:                  2024-01-26T04:26:30.140039-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con TOML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-toml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
TOML (Tom's Obvious, Minimal Language - El Lenguaje Mínimo y Obvio de Tom) es un formato de serialización de datos que es fácil de leer debido a su semántica clara. Los programadores usan TOML para archivos de configuración donde la legibilidad por humanos y el fácil análisis por máquinas son clave.

## Cómo hacerlo:
Para empezar, necesitas un analizador de TOML. Swift no tiene uno incorporado, entonces usemos `TOMLDecoder`. Instálalo a través del Administrador de Paquetes de Swift y luego serializa y deserializa TOML fácilmente.

```Swift
import TOMLDecoder

let tomlString = """
title = "Ejemplo TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Título: \(config.title), Propietario: \(config.owner.name), Fecha de Nacimiento: \(config.owner.dob)")
    } catch {
        print("Error al analizar TOML: \(error)")
    }
}
```

Este código produce la salida:
```
Título: Ejemplo TOML, Propietario: Tom Preston-Werner, Fecha de Nacimiento: 1979-05-27 07:32:00 +0000
```

## Análisis Profundo
TOML fue diseñado por Tom Preston-Werner, cofundador de GitHub, como una alternativa más amigable para humanos a formatos como JSON o YAML. Apunta hacia la claridad, reduciendo las posibilidades de malinterpretación tanto por un humano como por una máquina. En cuanto a alternativas, YAML y JSON son los sospechosos habituales, con YAML inclinándose hacia la legibilidad humana y JSON como la opción más simple y amigable para las máquinas. Al trabajar con TOML en Swift, no contamos con un analizador nativo. Sin embargo, bibliotecas de terceros como `TOMLDecoder` facilitan la conversión fácil entre cadenas TOML y tipos de Swift, específicamente a través de los protocolos `Codable` introducidos en Swift 4 que simplificaron la serialización.

## Ver También
- El estándar de TOML: https://toml.io
- GitHub para `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Documentación de Swift sobre `Codable`: https://developer.apple.com/documentation/swift/codable
- Comparación de formatos de serialización de datos: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
