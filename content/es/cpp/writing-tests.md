---
title:    "C++: Escribiendo pruebas"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en C++?

Escribir pruebas en C++ es una de las mejores prácticas en desarrollo de software. Permite a los programadores verificar el correcto funcionamiento de su código y asegurarse de que no hay errores no deseados. Además, proporciona una forma de control de calidad para garantizar que el código sea estable y confiable.

## Cómo escribir pruebas en C++

Para comenzar a escribir pruebas en C++, necesitará un marco de prueba. Uno de los marcos más populares es Google Test, que proporciona una estructura clara y organizada para escribir pruebas eficientes. Primero, debe definir una función de prueba utilizando la macro `TEST()` y luego utilizar las macros `ASSERT` dentro de ella para probar diferentes partes de su código. A continuación, se muestra un ejemplo de cómo se vería una prueba utilizando Google Test:

```C++
TEST(Calculadora, Suma) {
  // Setup
  int a = 3;
  int b = 5;
  
  // Operación
  int resultado = sumar(a, b);
  
  // Verificación
  ASSERT_EQ(resultado, 8);
}
```

En este ejemplo, se está probando una función `sumar` que toma dos números y devuelve su suma. Primero, se configuran los valores de prueba, luego se realiza la operación y finalmente se verifica que el resultado sea el esperado utilizando la macro `ASSERT_EQ()`. Puede agregar tantas pruebas como sean necesarias y ejecutarlas todas juntas para obtener un informe de resultados.

## Profundizando en las pruebas

Además de las pruebas unitarias como la que se muestra en el ejemplo anterior, también es importante realizar pruebas de integración y pruebas de sistema. Las pruebas de integración garantizan que las diferentes partes de su código se integren correctamente entre sí, mientras que las pruebas de sistema evalúan el comportamiento general del software.

Es importante tener en cuenta que las pruebas deben ser automáticas y repetibles para que puedan ejecutarse todas las veces que sea necesario para detectar cualquier problema en el código. Además, se recomienda realizar pruebas continuamente a medida que se va desarrollando el código para evitar acumular errores que puedan ser difíciles de rastrear y corregir más adelante.

## Ver también

- [Documentación de Google Test](https://github.com/google/googletest) 
- [Escribiendo pruebas unitarias en C++ con Google Test](https://www.freecodecamp.org/news/writing-tight-reliable-c-tests-with-googletest-part-i-a-simple-example-86e637db97d3/) 
- [Tutorial de C++ en Codeacademy](https://www.codecademy.com/learn/learn-c-plus-plus)