---
title:                "PHP: Escribiendo pruebas"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en PHP

Si eres un programador PHP, probablemente hayas escuchado hablar de pruebas y cómo son importantes en el proceso de desarrollo de software. Pero ¿por qué deberías hacer el esfuerzo de escribir pruebas? La respuesta es sencilla: las pruebas te ayudan a detectar y corregir errores en tu código antes de que lleguen a producción. Además, te permiten realizar cambios en tu código con confianza, ya que tendrás una forma de comprobar si tus cambios no han afectado negativamente al funcionamiento de tu aplicación.

## Cómo escribir pruebas en PHP

Escribir pruebas en PHP es una habilidad esencial que todos los programadores deberían tener. Afortunadamente, ¡es bastante sencillo! Vamos a ver un ejemplo de cómo escribir una prueba para una función de suma básica:

```PHP
<?php
function sumar($a, $b) {
    return $a + $b;
}
```

Primero, debemos incluir el framework de pruebas PHPUnit en nuestro proyecto. Una vez hecho esto, podemos escribir nuestra prueba de la siguiente manera:

```PHP
<?php
use PHPUnit\Framework\TestCase;

class FunctionsTest extends TestCase
{
    public function testSumar() {
        // Arrange
        $a = 3;
        $b = 5;

        // Act
        $resultado = sumar($a, $b);

        // Assert
        $this->assertEquals(8, $resultado);
    }
}
```

Aquí, estamos utilizando el método `assertEquals()` de PHPUnit para comprobar si la suma de nuestros dos números es igual a 8. Si el resultado no fuera igual, la prueba fallaría y nos indicaría que algo en nuestra función de suma está mal.

## Profundizando en la escritura de pruebas

Escribir pruebas puede ser mucho más que simplemente comprobar si el resultado de una función es correcto. Hay una serie de conceptos que pueden ayudarte a ser más efectivo al escribir pruebas, como las pruebas unitarias, las pruebas de integración y la cobertura del código. También es importante aprender a utilizar aserciones, mocks y datos de prueba para poder escribir pruebas más robustas y eficientes.

Puedes profundizar en estos temas a través de recursos como [esta guía de pruebas en PHP](https://www.php.net/manual/es/book.tester.php) o [este curso en línea sobre pruebas de software](https://www.udemy.com/course/pruebas-de-software/).

## Ver también

- [Documentación oficial de PHPUnit en español](https://phpunit.readthedocs.io/es/latest/)
- [Artículo sobre cómo escribir buenas pruebas en PHP](https://david.heinemann.me/blog/como-escribir-buenos-tests-en-php/)
- [Vídeo tutorial sobre cómo escribir pruebas unitarias en PHP](https://www.youtube.com/watch?v=KywcZs11PDI)