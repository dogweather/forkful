---
title:                "C: Escribiendo pruebas"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-tests.md"
---

{{< edit_this_page >}}

# ¿Por qué escribir pruebas en tus programas de C?

Antes de profundizar en cómo escribir pruebas en tus programas de C, es importante entender por qué es una práctica valiosa. Las pruebas son una parte esencial del desarrollo de software ya que ayudan a identificar y corregir errores antes de que el programa se lance a producción. Además, escribir pruebas te permite tener un mayor control sobre el funcionamiento de tu código y asegurarte de que sigue funcionando correctamente conforme se van realizando cambios.

## Cómo escribir pruebas en C

Para escribir pruebas en tus programas de C, necesitarás usar una librería de pruebas como [CUnit](http://cunit.sourceforge.net/) o [Check](https://libcheck.github.io/check/). Estas librerías te proporcionarán funciones y macros específicas para realizar diferentes tipos de pruebas.

Veamos un ejemplo de cómo escribir una prueba sencilla usando la librería CUnit:

```C
#include <CUnit/CUnit.h>

void test_suma() {
  int a = 5;
  int b = 10;
  int resultado = a + b;
  CU_ASSERT_EQUAL(resultado, 15);
}

int main() {
  CU_initialize_registry();
  CU_pSuite suite = CU_add_suite("Suite de pruebas", NULL, NULL);
  CU_add_test(suite, "Prueba de suma", test_suma);
  CU_basic_run_tests();
  CU_cleanup_registry();
  return 0;
}
```

En este ejemplo, estamos definiendo una función de prueba llamada `test_suma()` que verifica si la suma de dos números es igual al resultado esperado. Luego, en la función `main()`, inicializamos el registro de pruebas, creamos una suite de pruebas y agregamos nuestra función `test_suma()` a la suite. Finalmente, llamamos a `CU_basic_run_tests()` para ejecutar todas las pruebas y cerramos el registro con `CU_cleanup_registry()`.

Una vez que nuestra prueba se ejecute, la salida debería ser la siguiente:

```
Suite de pruebas
| Prueba de suma: Success
| 1 tests passed.
```

## Profundizando en la escritura de pruebas

Existen diferentes tipos de pruebas que puedes escribir en tus programas de C, como pruebas unitarias, de integración y de regresión. Cada tipo de prueba tiene su propósito y te ayuda a garantizar que tu código funcione como se espera.

Una recomendación importante es que escribas pruebas desde el principio del desarrollo del programa y las vayas actualizando conforme se realicen cambios en el código. De esta manera, tendrás una base sólida de pruebas que te darán confianza en el funcionamiento de tu programa.

Otro aspecto a tener en cuenta es la cobertura de las pruebas, es decir, qué porcentaje de tu código está siendo probado. Es importante tener una cobertura de pruebas lo más completa posible para asegurarnos de que todas las posibles ramas y casos de uso en nuestro código están siendo probados.

# Ver también

- [The Art of Unit Testing in C](https://www.freertos.org/Documentation/FreeRTOS_Support_Forum_Archive/April_2016/freertos_The-art-of-unit-testing-in-C-4sf28og7.html)
- [Writing tests for C projects with CppUTest](https://medium.com/@michaelchenyf/writing-tests-for-c-projects-with-cpputest-fd94597900dd)
- [Continuous Integration in C using GitHub Actions](https://dev.to/xintrea/c-continuous-integration-with-github-actions-42a2)