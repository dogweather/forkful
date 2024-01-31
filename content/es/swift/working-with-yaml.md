---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajamos con YAML porque es fácil de leer y escribir configuraciones o datos. Programadores usan YAML para serialización de datos, como en archivos de configuración para apps y servicios.

## Cómo hacerlo:

```Swift
// Ejemplo de cómo leer un archivo YAML en Swift
import Yams

let yaml = """
nombre: Juan
edad: 30
ciudad: Madrid
"""

do {
    // Parseamos el YAML a un diccionario
    if let data = try Yams.load(yaml: yaml) as? [String: Any] {
        print(data["nombre"] ?? "nombre desconocido")  // Output: Juan
        print(data["edad"] ?? "edad desconocida")      // Output: 30
        print(data["ciudad"] ?? "ciudad desconocida")  // Output: Madrid
    }
} catch {
    print("Error al leer YAML: \(error)")
}
```

## Inmersión Profunda:

YAML, que significa YAML Ain't Markup Language, inició en 2001. Es un superconjunto de JSON, ofreciendo una alternativa humanamente más legible. En Swift, solemos trabajar con librerías como Yams por su facilidad de integración. Otras opciones incluyen JSON y XML, pero YAML es preferido cuando la claridad es crucial.

## Ver También:

- Documentación oficial de YAML: https://yaml.org/
- Repo de Yams en GitHub: https://github.com/jpsim/Yams
- Comparativa de JSON vs. YAML: https://json2yaml.com/
