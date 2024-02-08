---
title:                "Escribiendo pruebas"
aliases:
- es/swift/writing-tests.md
date:                  2024-02-03T19:31:50.335244-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir pruebas en Swift implica crear y ejecutar código que verifica la corrección de otras unidades de código en tu aplicación. Los programadores lo hacen para asegurar la fiabilidad, detectar errores temprano en el ciclo de desarrollo y facilitar la futura refactorización del código sin consecuencias no intencionadas.

## Cómo hacerlo:
Swift soporta las pruebas a través de su marco XCTest, el cual está integrado en Xcode. Puedes escribir pruebas unitarias para verificar partes individuales de tu código, por ejemplo, una función que calcula la suma de dos números.

```swift
import XCTest
@testable import TuApp

class PruebasDeTuApp: XCTestCase {

    func pruebaSuma() {
        let resultado = Calculadora().suma(a: 1, b: 2)
        XCTAssertEqual(resultado, 3, "La función suma no devolvió el valor esperado.")
    }
}
```

Para ejecutar esta prueba, normalmente presionarías Command-U en Xcode. La salida en el navegador de pruebas de Xcode te dirá si la prueba pasó o falló.

Por ejemplo, una salida de prueba exitosa:
```
Test Case '-[PruebasDeTuApp pruebaSuma]' passed (0.005 segundos).
```

Para escenarios de pruebas más avanzados, podrías adoptar bibliotecas de terceros como Quick/Nimble, que ofrecen una sintaxis más expresiva para escribir pruebas.

Con Quick/Nimble, podrías escribir la misma prueba así:

```swift
// Añade Quick y Nimble a tu gestor de paquetes Swift o usa CocoaPods/Carthage para instalarlos
import Quick
import Nimble
@testable import TuApp

class EspecificacionCalculadora: QuickSpec {
    override func spec() {
        describe("Calculadora") {
            context("al sumar números") {
                it("debería devolver la suma correcta") {
                    let calculadora = Calculadora()
                    expect(calculadora.suma(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Ejecutar esta prueba te daría una salida similar en tu consola de prueba o en el registro de la herramienta CI/CD, indicando si la prueba tuvo éxito o falló, con un formato más legible para describir pruebas y expectativas.
