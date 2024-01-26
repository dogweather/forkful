---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir pruebas es validar que nuestro código hace lo que esperamos; evita sorpresas en el futuro. Los programadores las escriben para asegurar calidad, ahorrar tiempo de depuración y facilitar mantenimiento.

## Cómo hacerlo:

Usaremos `Google Test`, un framework popular para pruebas en C++. Primero, instalalo y configuralo en tu entorno. Aquí un test simple para una función que suma dos números.

```C++
// suma.h
int suma(int a, int b);

// suma.cpp
#include "suma.h"
int suma(int a, int b) {
    return a + b;
}

// test_suma.cpp
#include "suma.h"
#include <gtest/gtest.h>

TEST(TestSuma, CalculoBasico) {
    EXPECT_EQ(3, suma(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Compila con `g++ -std=c++17 test_suma.cpp suma.cpp -lgtest -lgtest_main -pthread -o test_suma` y luego ejecuta `./test_suma`. Deberías ver algo así:

```
[==========] Running 2 tests from 1 test case.
[----------] Global test environment set-up.
[----------] 1 test from TestSuma
[ RUN      ] TestSuma.CalculoBasico
[       OK ] TestSuma.CalculoBasico (0 ms)
[----------] 1 test from TestSuma (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test case ran. (1 ms total)
[  PASSED  ] 2 tests.
```

## Profundizando

La prueba de software no es nueva; ha evolucionado desde los 70. Frameworks como `Google Test` (C++), `JUnit` (Java) o `pytest` (Python), permiten estructurar pruebas de manera sencilla y eficaz. Algunos prefieren TDD (Test-Driven Development), donde las pruebas guían el diseño del código. Otras alternativas son Catch2 y Boost.Test para C++. Poco a poco cambia la implementación, pero el objetivo persiste: código confiable y mantenible.

## Ver También

Aquí algunos enlaces para aprender más:

- Google Test: https://github.com/google/googletest
- Catch2: https://github.com/catchorg/Catch2
- Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
- Guía de Google Test: https://google.github.io/googletest/quickstart-cmake.html

Estos recursos te darán un gran punto de partida y profundidad en la materia de pruebas de software en C++.
