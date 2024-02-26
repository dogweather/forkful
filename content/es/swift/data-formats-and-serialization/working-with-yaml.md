---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:50.053306-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un est\xE1ndar de serializaci\xF3n de datos amigable para el ser\
  \ humano\u2026"
lastmod: '2024-02-25T18:49:55.906395-07:00'
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un est\xE1ndar de serializaci\xF3n de datos amigable para el ser\
  \ humano\u2026"
title: Trabajando con YAML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), es un estándar de serialización de datos amigable para el ser humano utilizado en todos los lenguajes de programación. Los programadores lo utilizan para archivos de configuración, mensajería entre procesos y almacenamiento de datos porque su legibilidad es mucho más cercana al inglés llano comparado con otros formatos de datos como XML o JSON, lo que hace que sea más simple de entender y escribir.

## Cómo hacerlo:
Swift no incluye soporte integrado para el análisis y serialización de YAML, lo que hace necesario el uso de bibliotecas de terceros. Una opción popular es `Yams`, una biblioteca para trabajar con YAML en Swift.

Primero, necesitas agregar `Yams` a tu proyecto. Si usas Swift Package Manager, puedes agregarlo como una dependencia en tu archivo `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", desde: "4.0.0")
]
```

### Parseando YAML a Swift
Supón que tienes la siguiente configuración YAML para una aplicación simple:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

Así es cómo puedes parsear esta cadena YAML en Swift usando `Yams`:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // Ejemplo de acceso a los datos parseados
        if let name = data["name"] as? String {
            print("Nombre de la App: \(name)")
        }
    }
} catch {
    print("Error al parsear YAML: \(error)")
}
```

Salida de muestra:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
Nombre de la App: MyApp
```

### Serializando Objetos Swift a YAML
Convertir un objeto Swift de nuevo a una cadena YAML también es directo con `Yams`. Supongamos que tienes la misma estructura de datos que necesita ser serializada:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("Error al serializar a YAML: \(error)")
}
```

Esto producirá una cadena formateada en YAML:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

Estos ejemplos demuestran operaciones básicas para trabajar con YAML en aplicaciones Swift. Recuerda, aunque YAML sobresale en legibilidad humana y facilidad de uso, siempre considera las necesidades específicas de tu aplicación, especialmente con respecto al rendimiento y la complejidad, al elegir tu formato de serialización de datos.
