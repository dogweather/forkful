---
title:                "Escribiendo pruebas"
html_title:           "Swift: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas es una parte importante del proceso de desarrollo de software. Consiste en escribir código que prueba si nuestro código principal funciona correctamente. Los programadores lo hacen para asegurarse de que su código está libre de errores y funciona como se espera.

## Cómo:

```Swift
func sumar(_ a: Int, _ b: Int) -> Int {
    return a + b
}

func testSumar() {
    // Creamos un objeto de prueba
    let resultado = sumar(3, 5)
    // Comprobamos si el resultado es correcto
    assert(resultado == 8, "La suma debería dar 8")
}

// Ejecutamos la prueba
testSumar()

```

## Profundizar:

Escribir pruebas automatizadas se ha vuelto cada vez más popular en el desarrollo de software en los últimos años. Esto se debe a que permite a los desarrolladores encontrar y solucionar errores rápidamente, asegurando un producto final de alta calidad. También existen otras formas de probar el código, como pruebas manuales o pruebas de aceptación, pero las pruebas automatizadas son más eficientes y confiables.

Al escribir pruebas, es importante tener en cuenta que deben ser independientes y repetibles. Esto significa que una prueba no debe depender de otra y siempre debe dar el mismo resultado cada vez que se ejecute. También se pueden utilizar herramientas de pruebas como Xcode o XCTest para facilitar el proceso de escritura y ejecución de pruebas.

## Ver también:

- [Documentación oficial de Apple sobre pruebas en Swift](https://developer.apple.com/documentation/xctest)