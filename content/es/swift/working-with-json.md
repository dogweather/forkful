---
title:                "Trabajando con json"
html_title:           "Swift: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON en Swift?

¿Alguna vez te has preguntado cómo aplicaciones como Instagram y Twitter obtienen y muestran datos en tiempo real? La respuesta es JSON. JSON (JavaScript Object Notation) es un formato de texto ligero y sencillo de entender utilizado para intercambiar datos entre aplicaciones. En este artículo, aprenderás por qué JSON es tan importante en el desarrollo de aplicaciones y cómo utilizarlo en Swift.

## Cómo trabajar con JSON en Swift

Para trabajar con JSON en Swift, primero debes importar el framework `Foundation` en tu proyecto. Luego, puedes utilizar la clase `JSONSerialization` para convertir datos JSON en tipos de datos de Swift y viceversa.

```Swift
// Importar el framework Foundation
import Foundation

// Datos JSON de ejemplo
let jsonString = "{\"nombre\": \"María\", \"edad\": 25, \"ciudad\": \"Madrid\"}"
let jsonData = jsonString.data(using: .utf8)!

// Convertir datos JSON a una estructura Swift
if let json = try? JSONSerialization.jsonObject(with: jsonData, options: []) as? [String: Any] {
  // Obtener un valor específico
  let name = json["nombre"] as? String

  // Convertir una estructura Swift a datos JSON
  if let jsonData = try? JSONSerialization.data(withJSONObject: json, options: .prettyPrinted) {
    // Convertir datos JSON a una cadena de texto
    let jsonString = String(data: jsonData, encoding: .utf8)
    print(jsonString)
  }
}
```

Output:
```Swift
Optional("{\"nombre\":\"María\",\"ciudad\":\"Madrid\",\"edad\":25}")
```

## Profundizando en JSON

JSON es una forma sencilla y legible de estructurar datos para su intercambio entre aplicaciones. Utiliza una combinación de pares de clave-valor y listas ordenadas para almacenar información. En Swift, estos datos se convierten en diccionarios y arrays respectivamente. Además, JSON permite anidar estructuras, lo que lo hace flexible y escalable.

Una cosa importante a tener en cuenta al trabajar con JSON es que es sensible a mayúsculas y minúsculas, por lo que los nombres de las claves deben ser escritos exactamente igual en ambas aplicaciones. También puedes utilizar herramientas en línea como [JSON Formatter](https://jsonformatter.org/) para validar y formatear datos JSON.

## Ver también

- [Documentación oficial de Apple sobre JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Tutorial de Ray Wenderlich sobre JSON en Swift](https://www.raywenderlich.com/3418439-json-tutorial-in-swift-getting-started)
- [Tutorial de Hacking With Swift sobre JSON en Swift](https://www.hackingwithswift.com/articles/153/how-to-parse-json-using-jsonserialization)