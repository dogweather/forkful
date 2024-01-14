---
title:    "PHP: Escritura de pruebas"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-tests.md"
---

{{< edit_this_page >}}

# Por qué deberías escribir pruebas en PHP

Al escribir código en PHP, es común que surjan errores y bugs durante el proceso de desarrollo. Para evitar que estos errores afecten a la funcionalidad de tu aplicación, es importante escribir pruebas unitarias. Estas pruebas te permiten detectar y corregir errores de manera eficiente, lo que a su vez ayuda a mejorar la calidad y seguridad de tu código.

## Cómo escribir pruebas en PHP

Para escribir pruebas en PHP, puedes utilizar una herramienta llamada PHPUnit. Comienza por instalar PHPUnit en tu proyecto usando Composer, una herramienta popular para manejar las dependencias en PHP.

Una vez que tienes PHPUnit instalado, puedes crear tus pruebas dentro de la carpeta `tests` en tu proyecto. Por ejemplo, si quieres escribir una prueba para una función llamada `sumar` en un archivo `functions.php`, puedes crear un archivo llamado `functionsTest.php` en la carpeta `tests` y escribir el siguiente código:

```PHP
use PHPUnit\Framework\TestCase;

class FunctionsTest extends TestCase
{
    public function testSumar()
    {
        include 'functions.php';

        // Arrange
        $num1 = 10;
        $num2 = 5;

        // Act
        $resultado = sumar($num1, $num2);

        // Assert
        $this->assertEquals(15, $resultado);
    }
}
```

En este ejemplo, estamos importando el archivo `functions.php` y probando la función `sumar` usando la aserción `assertEquals` para verificar que el resultado sea igual a 15.

Una vez que tengas tus pruebas escritas, puedes ejecutarlas usando el comando `phpunit` desde la línea de comandos. Verás una salida como esta:

```
PHPUnit 6.0.4 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 152 ms, Memory: 4.00MB

OK (1 test, 1 assertion)
```

Esta salida indica que la prueba ha pasado con éxito. Si ocurre algún error, PHPUnit te proporcionará información detallada para que puedas corregirlo.

## Profundizando en la escritura de pruebas

Escribir pruebas en PHP no se trata solo de verificar que tus funciones produzcan los resultados esperados. Se trata también de cubrir todos los posibles escenarios y situaciones para garantizar que tu código sea robusto y confiable.

Una buena práctica es seguir la metodología AAA (Arrange, Act, Assert) para escribir pruebas claras y bien estructuradas. También debes tener en cuenta el uso de mocks y stubs para simular diferentes situaciones y evitar depender de datos externos en tus pruebas.

Otra cosa importante a tener en cuenta es escribir pruebas para todas las funcionalidades de tu código, incluso para aquellas que puedan parecer triviales. De esta manera, puedes estar seguro de que cualquier cambio que hagas en tu código no romperá nada.

# Ver también

- [Documentación de PHPUnit](https://phpunit.de/documentation.html)
- [Tutorial de pruebas unitarias en PHP](https://www.tutorialspoint.com/phpunit/index.htm)
- [Artículo sobre la metodología AAA en pruebas unitarias](https://medium.com/@ajolvi/the-aaa-pattern-in-unit-tests-7115706802d0)