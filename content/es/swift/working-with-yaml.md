---
title:                "Trabajando con yaml"
html_title:           "Swift: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

Qué es YAML y por qué los programadores lo usan
--------------------------------------------------

YAML es un formato de serialización de datos, lo que significa que es una forma de representar información en un archivo legible tanto para humanos como para máquinas. Los programadores usan YAML para almacenar y compartir datos estructurados, como configuraciones de aplicaciones o datos de prueba para realizar pruebas automatizadas.

Cómo utilizar YAML en Swift
---------------------------

Para trabajar con YAML en Swift, primero necesitamos importar la biblioteca `Yams` en nuestro proyecto. Luego, podemos utilizar el método `YAMLDecoder()` para decodificar un archivo YAML en un objeto Swift, o el método `YAMLEncoder()` para codificar un objeto Swift en un archivo YAML.

```
// Ejemplo de decodificación de un archivo YAML:
let yamlString = """
todos:
  - ir de compras
  - cocinar la cena
  - hacer ejercicio
"""
let decoder = YAMLDecoder()
let todos = try decoder.decode([String].self, from: yamlString)
print(todos)
// output: ["ir de compras", "cocinar la cena", "hacer ejercicio"]

// Ejemplo de codificación de un objeto Swift:
struct Empleado: Codable {
  let nombre: String
  let departamento: String
}
let empleado = Empleado(nombre: "María", departamento: "Ventas")
let encoder = YAMLEncoder()
let yamlString = try encoder.encode(empleado)
print(yamlString)
// output: "nombre: María\ndepartamento: Ventas\n"
```

Una mirada más profunda a YAML
------------------------------

YAML fue creado originalmente por Clark Evans en 2001 y hoy en día es un estándar ampliamente utilizado en la comunidad de programadores. Además de ser fácil de leer y escribir, YAML también es útil porque es independiente del lenguaje, lo que significa que se puede utilizar con cualquier lenguaje de programación.

Si bien YAML es una opción popular para almacenar y compartir datos estructurados, también hay otras alternativas como JSON y XML. Cada formato tiene sus propias ventajas y desventajas, por lo que es importante considerar cuál es el mejor para tu proyecto en particular.

En términos de implementación, YAML en Swift requiere el uso de bibliotecas externas como `Yams` o `YamlSwift`, ya que no hay soporte nativo para YAML en el lenguaje. Sin embargo, estas bibliotecas facilitan mucho el proceso de trabajar con YAML en Swift.

Más recursos sobre YAML
-----------------------

Si quieres aprender más sobre YAML y cómo usarlo en tus proyectos de Swift, aquí te dejamos algunos enlaces útiles:

- [Documentación oficial de YAML](https://yaml.org/)
- [Biblioteca Swift para trabajar con YAML](https://github.com/jpsim/Yams)
- [Tutorial de YAML para principiantes](https://www.codeproject.com/Articles/1214409/Learn-YAML-in-five-minutes)

¡Ahora estás listo para empezar a trabajar con YAML en Swift! Recuerda siempre considerar los requisitos y necesidades de tu proyecto antes de decidir qué formato utilizar para almacenar tus datos estructurados.