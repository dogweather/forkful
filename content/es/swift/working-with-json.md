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

## ¿Qué y por qué?

Trabajar con JSON es una parte importante del desarrollo de aplicaciones, ya que es un formato de intercambio de datos muy utilizado en la web. Los programadores lo usan para convertir datos estructurados en objetos de programación y viceversa.

## ¡A por ello!

Para trabajar con JSON en Swift, primero debemos importar la librería `Foundation` en nuestro código. Luego, podemos usar el método `JSONSerialization` para convertir datos JSON en objetos de Swift. Por ejemplo:

```Swift
import Foundation

let jsonString = "{\"name\": \"María\", \"age\": 25}"
let jsonData = jsonString.data(using: .utf8)!

do {
    if let jsonObject = try JSONSerialization.jsonObject(with: jsonData, options: .allowFragments) as? [String: Any] {
        let name = jsonObject["name"] as! String
        let age = jsonObject["age"] as! Int
        print("\(name) tiene \(age) años.")
    }
} catch {
    print(error)
}

// Output: María tiene 25 años.
```

## Detalles en profundidad

JSON fue desarrollado en los años 90 como una alternativa a XML y se ha convertido en un formato muy popular debido a su simplicidad y a su facilidad de lectura para humanos y máquinas. Además de `JSONSerialization`, también existen otras librerías, como `SwiftyJSON` y `ObjectMapper`, que hacen que trabajar con JSON en Swift sea aún más fácil.

## Vea también

- [Documentación oficial de JSON en Swift](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Librería SwiftyJSON para trabajar con JSON en Swift](https://github.com/SwiftyJSON/SwiftyJSON)
- [Librería ObjectMapper para mapear objetos Swift a JSON y viceversa](https://github.com/tristanhimmelman/ObjectMapper)