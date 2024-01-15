---
title:                "Creando pruebas"
html_title:           "C++: Creando pruebas"
simple_title:         "Creando pruebas"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en C++

Escribir pruebas en el código es una práctica común entre los programadores para asegurar la calidad y funcionalidad del código. Al escribir pruebas, podemos identificar y corregir errores en nuestro código antes de que se conviertan en un problema para el usuario final. Además, tener pruebas también facilita el proceso de depuración y mejora la eficiencia en el desarrollo.

## Cómo escribir pruebas en C++

Para escribir pruebas en C++, podemos utilizar la biblioteca de pruebas integrada en el framework de Google Test. Esta biblioteca nos permite crear pruebas unitarias para diferentes funciones y clases en nuestro código. Veamos un ejemplo de cómo escribir y ejecutar una prueba en C++:

```
#include <iostream>
#include "gtest/gtest.h"

// Función a probar
int sum(int a, int b) {
    return a + b;
}

// Definiendo una prueba
TEST(SumTest, PositiveNumbers) {
    // Arrange - Se preparan los datos necesarios para la prueba
    int a = 5;
    int b = 3;

    // Act - Se llama a la función que se quiere probar
    int result = sum(a, b);

    // Assert - Se comprueba si el resultado es el esperado
    ASSERT_EQ(result, 8);
}

int main() {
    // Inicializar Google Test
    testing::InitGoogleTest();

    // Ejecutar todas las pruebas
    return RUN_ALL_TESTS();
}
```

El código anterior define una función `sum` que suma dos números enteros y una prueba que verifica si la función funciona correctamente para números positivos. Al ejecutar la prueba, deberíamos obtener una salida como la siguiente:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from SumTest
[ RUN      ] SumTest.PositiveNumbers
[       OK ] SumTest.PositiveNumbers (0 ms)
[----------] 1 test from SumTest (0 ms total)

[----------] Global test environment tear-down
[==========] 1 test from 1 test suite ran. (0 ms total)
```

Este es sólo un ejemplo simple de cómo escribir una prueba en C++. Con la biblioteca de Google Test, también podemos realizar pruebas más complejas e incluso pruebas de integración.

## Profundizando en la escritura de pruebas en C++

Hay varios conceptos importantes que debemos tener en cuenta al escribir pruebas en C++. A continuación, mencionaremos algunos de ellos:

- **Cobertura de código:** esto se refiere a la cantidad de código que está siendo probado por nuestras pruebas. El objetivo es tener una cobertura de código cercana al 100% para asegurar que todas las partes de nuestro código estén siendo probadas.
- **Mocking:** es una técnica para simular comportamientos en nuestras pruebas. Esto puede ser útil cuando tenemos dependencias externas en nuestro código, como llamadas a una base de datos o servicios web.
- **Refactorización de pruebas:** al igual que en el código, también es importante mantener nuestras pruebas limpias y bien organizadas. Utilizar buenas prácticas de refactorización de código para nuestras pruebas nos ayudará a mantener un conjunto de pruebas legible y fácil de mantener.

Para profundizar más en la escritura de pruebas en C++, se recomienda explorar documentación adicional y tutoriales en línea.

## Ver también

- [Google Test](https://github.com/google/googletest)
- [Introducción a las pruebas unitarias en C++](https://www.inf.utfsm.cl/~rvalenzu/TDD/UnitTestingInCPP.pdf)
- [Tutorial de Google Test en C++](https://github.com/google/googletest/blob/master/googletest/docs/primer.md)