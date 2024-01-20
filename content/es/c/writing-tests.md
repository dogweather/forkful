---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué? 
Escribir pruebas en programación es crear código que verifica que otro código funciona correctamente. Los programadores lo hacen para asegurar calidad, evitar errores y facilitar el mantenimiento del software.

## Cómo Hacerlo: 
Para demostrar, utilizaremos la biblioteca más común en C para pruebas unitarias, llamada CUnit. Aquí hay un ejemplo simple de cómo escribir una prueba para una función que suma dos números:

```C
#include <CUnit/Basic.h>
#include <assert.h>

// La función que queremos probar
int suma(int a, int b) {
    return a + b;
}

// La prueba unitaria para la función suma
void test_suma() {
    CU_ASSERT(5 == suma(2, 3)); // Afirmamos que 2 + 3 debe ser 5
}

int main() {
    CU_initialize_registry();
    CU_pSuite suite = CU_add_suite("Suite de Prueba", 0, 0);
    
    CU_add_test(suite, "Test para la función suma", test_suma);
    
    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CU_cleanup_registry();
    
    return 0;
}
```

La salida debería ser algo como:

```
Suite de Prueba 
    Test para la función suma: Passed
```

## Profundización:
Las pruebas unitarias en C han evolucionado desde simples aserciones (asserts) hasta marcos de prueba más avanzados. Alternativas a CUnit incluyen Check, Unity y CMock. Estos ofrecen más funcionalidades, tales como simulaciones y ejecución de pruebas en paralelo. Es importante elegir la herramienta adecuada dependiendo de la complejidad del proyecto y los requisitos específicos.

## Ver También:
- CUnit: http://cunit.sourceforge.net/
- Check: https://libcheck.github.io/check/
- Unity: http://www.throwtheswitch.org/unity
- CMock: http://www.throwtheswitch.org/cmock