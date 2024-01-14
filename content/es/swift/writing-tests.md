---
title:    "Swift: Escribiendo pruebas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-tests.md"
---

{{< edit_this_page >}}

# ¿Por qué escribir pruebas en Swift?

Escribir pruebas es una parte esencial del proceso de programación. Las pruebas nos permiten asegurarnos de que nuestro código funciona como debería, evitando errores y garantizando una mayor calidad en nuestras aplicaciones. Además, nos permiten detectar y solucionar problemas antes de que lleguen a la etapa de producción.

## Cómo escribir pruebas en Swift

Para escribir pruebas en Swift, utilizamos el framework XCTest incorporado en Xcode. Este framework nos proporciona herramientas para crear casos de prueba y comprobar su resultado.

Para comenzar a escribir pruebas, debemos seguir los siguientes pasos:

1. Agregar un nuevo archivo de prueba a nuestro proyecto, seleccionando File > New > File y eligiendo la opción "Unit Testing Bundle" en la sección "Test" del menú desplegable.
2. Importar XCTest en nuestro nuevo archivo de prueba.
3. Crear un método que comience con la palabra "test", que será nuestro caso de prueba.
4. Dentro de este método, utilizamos las aserciones (assertions) para comprobar si una determinada parte de nuestro código cumple con las condiciones esperadas.
5. Ejecutar las pruebas utilizando el botón "Play" en la barra de herramientas de Xcode.

A continuación, un ejemplo de un caso de prueba que verifica si la suma de dos números es correcta:

```Swift
func testSuma() {
    //Arrange
    let num1 = 5
    let num2 = 3
    
    //Act
    let resultado = num1 + num2
    
    //Assert
    XCTAssertEqual(resultado, 8)
}
```

## Profundizando en la escritura de pruebas

Además de las aserciones básicas, XCTest también nos ofrece herramientas para realizar pruebas más complejas, como por ejemplo, la ejecución de tareas en diferentes hilos de ejecución o la medición de rendimiento de nuestro código.

Otra práctica recomendada es utilizar el concepto de "prueba unitaria", que consiste en probar cada una de las funciones o métodos de nuestro código de forma aislada, para asegurarnos de que cada una de ellas funciona correctamente.

Es importante también seguir el principio de "Arrange, Act, Assert", que nos indica que debemos preparar los datos de entrada, ejecutar una acción y luego comprobar el resultado obtenido.

## Ver también

- [Documentación de XCTest en Swift](https://developer.apple.com/documentation/xctest)
- [Tutorial: Writing Tests in Swift](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial) (en inglés)
- [Artículo: Los beneficios de escribir pruebas en Swift](https://medium.com/flawless-app-stories/the-benefits-of-writing-tests-in-swift-f7cd3cba9806) (en inglés)