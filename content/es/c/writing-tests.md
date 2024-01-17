---
title:                "Escribiendo pruebas"
html_title:           "C: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-tests.md"
---

{{< edit_this_page >}}

**¿Qué y por qué escribir pruebas en C?**

Escribir pruebas en C es una práctica común entre los programadores para verificar la funcionalidad de sus códigos. Las pruebas son código adicional que se escribe para evaluar si una función o programa está funcionando correctamente. Esto ayuda a detectar errores temprano y garantizar que el código sea robusto y confiable.

**Cómo hacerlo:**

Para escribir pruebas en C, utilizamos la biblioteca estándar `assert.h`. Esta biblioteca nos proporciona la función `assert` que evalúa una expresión y, si es falsa, termina el programa con un mensaje de error. Veamos un ejemplo:

```C
#include <stdio.h>
#include <assert.h>

int main() {
    int x = 5;
    int y = 6;

    assert(x == y); // Esta prueba fallará y el programa terminará

    return 0;
}
```

El resultado de este código sería:

```
Assertion failed: (x == y), function main, file test.c, line 8
```

Como podemos ver, `assert` nos proporciona información útil sobre dónde falló la prueba y qué expresión fue evaluada al momento del error.

**Profundizando:**

La práctica de escribir pruebas no es nueva y ha sido ampliamente adoptada por los programadores de C debido a su eficacia para detectar errores en códigos complejos. Alternativas a la biblioteca `assert.h` incluyen herramientas de pruebas automatizadas como Check, Unity y CUnit. Estas herramientas proporcionan una interfaz más amigable para escribir y ejecutar pruebas. 

Si bien las pruebas son efectivas para detectar errores, no garantizan que nuestro código sea completamente libre de errores. Además, escribir pruebas puede ser una tarea tediosa y requiere tiempo adicional. Sin embargo, en general, el beneficio de detectar errores temprano y garantizar un código robusto supera las desventajas.

**Ver también:**

- [Documentación de la biblioteca assert.h de C](https://www.tutorialspoint.com/c_standard_library/assert_h.htm)
- [Tutoriales y ejemplos de CUnit](http://cunit.sourceforge.net/)
- [Check, una herramienta de pruebas para C](https://libcheck.github.io/check/doc/check_html/check_3.html)