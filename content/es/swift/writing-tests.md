---
title:                "Swift: Creando pruebas"
simple_title:         "Creando pruebas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-tests.md"
---

{{< edit_this_page >}}

# Por qué escribir pruebas en Swift

Escribir pruebas es una parte esencial del desarrollo de cualquier aplicación, y esto también aplica a Swift. Las pruebas nos permiten asegurarnos de que nuestro código funciona correctamente y que cualquier cambio que hagamos no afectará negativamente a la funcionalidad existente. Además, escribir pruebas nos ayuda a encontrar y corregir errores de manera más eficiente, lo que ahorra tiempo y evita problemas en el futuro.

## Cómo escribir pruebas en Swift

Para escribir pruebas en Swift, podemos utilizar el framework de pruebas integrado en Xcode. Aquí hay un ejemplo sencillo de cómo escribir una prueba para una función que suma dos números:

```Swift 
func sumar(_ a: Int, _ b: Int) -> Int {
  return a + b
}

// Prueba
func testSumar() {
  let resultado = sumar(2, 3)
  assert(resultado == 5)
}
```

En este ejemplo, utilizamos la función `assert` para verificar que el resultado de nuestra función `sumar` sea igual a 5, que es el resultado esperado de sumar 2 y 3. Si la prueba falla, obtendremos un mensaje de error que nos indicará qué parte de nuestro código necesita ser revisada.

Al escribir pruebas, es importante asegurarse de cubrir todos los casos posibles y tener una buena cobertura de código. Podemos utilizar diferentes técnicas como pruebas unitarias, pruebas de integración y pruebas de interfaz de usuario para lograr una buena cobertura.

## Profundizando en la escritura de pruebas

Escribir pruebas no solo se trata de verificar el resultado de una función o un método, sino también de anticipar y prevenir posibles problemas. A veces, escribir pruebas nos obliga a analizar y entender mejor nuestro propio código y esto a su vez nos ayuda a mejorarlo. Además, el uso de pruebas en nuestro flujo de trabajo nos permite detectar fácilmente cualquier efecto secundario no deseado que pueda surgir al modificar nuestro código.

Otra ventaja de escribir pruebas es que nos permite documentar nuestro código. Al escribir pruebas, estamos describiendo cómo se supone que debe funcionar nuestro código, lo cual es muy útil para otros programadores que quieran entender y trabajar con nuestro código en el futuro.

# Ver también

- [Documentación de Xcode en español](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/es-lproj/00-Introduction/Introduction.html)
- [Tutorial de escritura de pruebas en Swift](https://www.raywenderlich.com/960290-getting-started-with-ios-unit-testing-in-swift)
- [Guía de mejores prácticas para escribir pruebas en Swift](https://medium.com/cr8resume/take-a-step-towards-swift-ios-test-driven-development-139a7c994873)