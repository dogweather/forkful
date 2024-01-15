---
title:                "Programando pruebas"
html_title:           "C: Programando pruebas"
simple_title:         "Programando pruebas"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué 

Escribir pruebas en tu código puede ser una tarea tediosa y que consuma tiempo, pero en realidad es una práctica muy valiosa. Las pruebas pueden ayudar a detectar errores y mejorar la calidad de tu código. Además, te brindan la confianza de que tu programa funciona correctamente antes de ponerlo en producción.

## Cómo hacerlo

Primero, es necesario entender qué es una prueba. Una prueba es simplemente una pieza de código que comprueba si otra pieza de código (generalmente una función) se comporta de la manera esperada. Hay diferentes tipos de pruebas, como pruebas unitarias y pruebas de integración, pero aquí nos enfocaremos en pruebas unitarias usando la biblioteca estándar de C, `assert.h`.

Para empezar, vamos a crear una función simple que sume dos números enteros y devuelva el resultado:

```C
int suma(int x, int y) {
    return x + y;
}
```

Ahora, vamos a escribir una prueba para esta función utilizando `assert.h`:

```C
#include <stdio.h>
#include <assert.h>

int suma(int x, int y);

int main(void) {
    int resultado = suma(5, 7);
    assert(resultado == 12);
    printf("La suma funciona correctamente!");
    return 0;
}
```

Si compilamos y ejecutamos este código, deberíamos ver el siguiente resultado:

```
La suma funciona correctamente!
```

En este ejemplo, hemos utilizado `assert` para comprobar si la suma de 5 y 7 es igual a 12, que es el resultado esperado. Si la aserción resulta ser falsa, el programa terminará inmediatamente y mostrará un mensaje de error. Esto nos indica que algo no está funcionando correctamente y nos permite corregir el problema antes de poner nuestro código en producción.

Es importante destacar que también es posible escribir pruebas antes de escribir la función en sí. De hecho, esta es una práctica común en la metodología de desarrollo impulsada por pruebas (Test Driven Development, o TDD). Al escribir las pruebas primero, puedes asegurarte de que tu función funciona correctamente desde el principio.

## Profundizando

Escribir pruebas no solo ayuda a mejorar la calidad de tu código, sino que también puede hacer que te conviertas en un mejor programador. Al escribir pruebas, te fuerzas a pensar en cómo tu código se comportará en diferentes escenarios y a anticiparte a posibles problemas. Además, te obliga a escribir un código más modular y fácil de mantener.

Hay muchas otras bibliotecas de pruebas disponibles para C, como Unity y cmocka, que pueden ser útiles para proyectos más complejos. También es importante tener en cuenta que escribir pruebas no es una solución milagrosa y no garantiza que no habrá errores en tu código, pero sin duda puede ayudar a detectarlos y solucionarlos más fácilmente.

## Ver también

- [Assert.h en la documentación de GNU](https://www.gnu.org/software/libc/manual/html_node/Assert.html)
- [Unity: Framework de pruebas para C](https://www.throwtheswitch.org/unity)
- [Cmocka: Biblioteca de pruebas unitarias para C](https://cmocka.org/)