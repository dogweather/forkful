---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:08.478122-07:00
description: "C\xF3mo hacerlo: #."
lastmod: '2024-03-13T22:44:59.378829-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Escribiendo pruebas
weight: 36
---

## Cómo hacerlo:


### Usando el Framework de Google Test
Una de las bibliotecas de terceros más populares para escribir pruebas en C++ es Google Test. Primero, necesitarás instalar Google Test y enlazarlo con tu proyecto. Una vez configurado, puedes comenzar a escribir casos de prueba.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Guarda el código en un archivo y compílalo con el compilador g++, enlazando la biblioteca de Google Test. Si todo está configurado correctamente, ejecutar el ejecutable resultante ejecutará la prueba, y si la función `add` funciona como se espera, verás algo como:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Usando Catch2
Otro framework de pruebas popular para C++ es Catch2. Tiene una sintaxis más simple y usualmente no requiere enlazarse contra una biblioteca (solo encabezado). Aquí hay un ejemplo de cómo escribir una prueba simple con Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // Esto le dice a Catch que proporcione un main() - solo hazlo en un archivo cpp
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Los enteros se multiplican", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Al compilar y ejecutar esta prueba, Catch2 proporciona una salida clara indicando si la prueba pasó o falló, junto con cualquier información necesaria para depurar fallas:

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

Estos ejemplos muestran cómo la integración de frameworks de pruebas en tu flujo de trabajo de desarrollo en C++ puede mejorar significativamente la fiabilidad y mantenibilidad de tu código.
