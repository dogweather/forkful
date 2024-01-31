---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Las expresiones regulares, o "regex", buscan patrones en texto. Los programadores las usan para validar, encontrar, o sustituir datos, de forma rápida y flexible.

## Cómo:
```Swift
import Foundation

let texto = "correo1@ejemplo.com, correo2@ejemplo.com"
let regex = try! NSRegularExpression(pattern: "[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z]{2,}")
let resultados = regex.matches(in: texto, range: NSRange(texto.startIndex..., in: texto))

let correos = resultados.map {
    String(texto[Range($0.range, in: texto)!])
}
print(correos)
// Salida: ["correo1@ejemplo.com", "correo2@ejemplo.com"]
```

## Profundización
Las regex existen desde los 1950s. Alternativas incluyen librerías de parsing, como Swift’s Scanner, pero a menudo son menos flexibles. NSRegularExpression es la implementación en Swift, tomando ventaja de la base de Objective-C.

## Ver También
- [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift String](https://developer.apple.com/documentation/swift/string)
- [Regular Expressions in Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)
