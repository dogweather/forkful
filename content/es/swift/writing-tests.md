---
title:                "Escribiendo pruebas"
date:                  2024-01-19
simple_title:         "Escribiendo pruebas"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Escribir pruebas es crear código que verifica que otro código funcione correctamente. Los programadores las realizan para asegurar la calidad, prevenir errores y facilitar la mantenibilidad del código.

## Cómo hacerlo
Ejemplo de una prueba unitaria en Swift usando XCTest:

```Swift
import XCTest
@testable import MiApp

class MiAppTests: XCTestCase {
    func testSumaCorrecta() {
        let resultado = Calculadora().suma(2, 3)
        XCTAssertEqual(resultado, 5, "La suma de 2 + 3 debería ser 5")
    }
}

class Calculadora {
    func suma(_ a: Int, _ b: Int) -> Int {
        return a + b
    }
}
```

Se ejecuta la prueba y el resultado esperado se muestra así:
```
Test Case '-[MiAppTests testSumaCorrecta]' passed (0.001 seconds).
```

## Análisis Profundo
Las pruebas de software son una práctica estándar desde los primeros días de la programación. Con alternativas como TDD (Desarrollo guiado por pruebas) donde las pruebas se escriben antes del código funcional o BDD (Desarrollo guiado por comportamiento) que se centra en el comportamiento del usuario y los casos de uso. Swift utiliza XCTest para pruebas unitarias, que se integra fácilmente con su herramienta de construcción Xcode.

## Ver También
- [Documentación oficial de XCTest](https://developer.apple.com/documentation/xctest)
- [Guía de pruebas unitarias de Swift](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
