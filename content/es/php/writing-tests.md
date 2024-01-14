---
title:                "PHP: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en PHP

Escribir pruebas en PHP es fundamental para garantizar la calidad de nuestro código. Con las pruebas, podemos detectar rápidamente errores y asegurarnos de que nuestro código funciona como debería. También nos permite hacer cambios en el futuro sin temor a romper el código existente. ¡Sigue leyendo para aprender cómo escribir pruebas en PHP!

## Cómo escribir pruebas en PHP

Primero, necesitamos asegurarnos de tener instalado PHPUnit, la herramienta de pruebas más popular para PHP. Una vez instalado, podemos comenzar a escribir nuestras pruebas. A continuación, se muestra un ejemplo de código de una función de suma simple:

```PHP
// Definimos una función para sumar dos números
function sumar($num1, $num2) {
    return $num1 + $num2;
}

// Escribimos nuestra prueba utilizando la aserción (assert) de PHPUnit
// Esta prueba verificará si la suma de 2 y 3 es igual a 5
use PHPUnit\Framework\TestCase;

class SumaTest extends TestCase {
    public function testSuma() {
        $resultado = sumar(2, 3);
        $this->assertEquals(5, $resultado);
    }
}
```

Al ejecutar esta prueba, si todo está bien, obtendremos una salida que nos indica que nuestra prueba ha pasado. Si quisieras probar una solución alternativa, por ejemplo, sumando dos números en lugar de restarlos, la prueba fallaría y nos mostraría un mensaje de error.

## Profundizando en las pruebas

Es importante tener en cuenta que nuestras pruebas deben ser lo más exhaustivas y completas posible. Podemos probar diferentes combinaciones de entradas y comparar los resultados esperados con los obtenidos. También podemos comprobar situaciones de error y asegurarnos de que nuestro código maneje adecuadamente esas situaciones.

Otra técnica útil para escribir pruebas es la prueba de "caja negra". Esto significa que probamos nuestra función solo desde la perspectiva del usuario, sin mirar el código interno. De esta manera, podemos asegurarnos de que nuestro código funcione correctamente desde el punto de vista del usuario final.

## Ver también

- [Documentación de PHPUnit](https://phpunit.readthedocs.io/en/9.5/)
- [Tutorial de pruebas en PHP](https://www.tutorialspoint.com/phpunit/phpunit_writing_tests.htm)
- [Guía de buenas prácticas para escribir pruebas en PHP](https://github.com/marco-pivetta/php-unit-best-practices)