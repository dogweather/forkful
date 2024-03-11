---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:17.430741-07:00
description: "Escribir pruebas en programaci\xF3n implica crear y ejecutar scripts\
  \ que verifican que el c\xF3digo se comporte como se espera bajo diversas condiciones.\
  \ Los\u2026"
lastmod: '2024-03-11T00:14:32.980698-06:00'
model: gpt-4-0125-preview
summary: "Escribir pruebas en programaci\xF3n implica crear y ejecutar scripts que\
  \ verifican que el c\xF3digo se comporte como se espera bajo diversas condiciones.\
  \ Los\u2026"
title: Escribiendo pruebas
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir pruebas en programación implica crear y ejecutar scripts que verifican que el código se comporte como se espera bajo diversas condiciones. Los programadores lo hacen para asegurar la calidad, prevenir regresiones y facilitar la refactorización segura, lo cual es crucial para mantener una base de código saludable, escalable y libre de errores.

## Cómo hacerlo:
### PHP Nativo – PHPUnit
Una herramienta ampliamente utilizada para pruebas en PHP es PHPUnit. Instálalo vía Composer:
```bash
composer require --dev phpunit/phpunit ^9
```

#### Escribiendo una prueba simple:
Crea un archivo `CalculatorTest.php` en un directorio `tests`:
```php
use PHPUnit\Framework\TestCase;

// Asumiendo que tienes una clase Calculator que suma números
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Ejecuta las pruebas con:
```bash
./vendor/bin/phpunit tests
```

#### Salida de muestra:
```
PHPUnit 9.5.10 por Sebastian Bergmann y contribuyentes.

.                                                                   1 / 1 (100%)

Tiempo: 00:00.005, Memoria: 6.00 MB

OK (1 prueba, 1 afirmación)
```

### Bibliotecas de Terceros – Mockery
Para pruebas complejas, incluyendo el mocking de objetos, Mockery es una opción popular.

```bash
composer require --dev mockery/mockery
```

#### Integrando Mockery con PHPUnit:
```php
use PHPUnit\Framework\TestCase;
use Mockery as m;

class ServiceTest extends TestCase
{
    public function tearDown(): void
    {
        m::close();
    }

    public function testServiceCallsExternalService()
    {
        $externalServiceMock = m::mock(ExternalService::class);
        $externalServiceMock->shouldReceive('process')->once()->andReturn('mocked result');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('mocked result', $result);
    }
}
```
Para ejecutar, utiliza el mismo comando de PHPUnit que arriba. Mockery permite objetos mock expresivos y flexibles, facilitando la prueba de interacciones complejas dentro de tu aplicación.
