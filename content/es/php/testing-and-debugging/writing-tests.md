---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:17.430741-07:00
description: "C\xF3mo hacerlo: Una herramienta ampliamente utilizada para pruebas\
  \ en PHP es PHPUnit. Inst\xE1lalo v\xEDa Composer."
lastmod: '2024-03-13T22:44:59.164353-06:00'
model: gpt-4-0125-preview
summary: Una herramienta ampliamente utilizada para pruebas en PHP es PHPUnit.
title: Escribiendo pruebas
weight: 36
---

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
