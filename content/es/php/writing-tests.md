---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Escribir tests es crear scripts que automáticamente verifican si tu código funciona como debería. Los desarrolladores lo hacen para asegurarse de que su código sea confiable y para evitar futuros errores al modificarlo.

## Cómo hacerlo:

Con PHPUnit, una herramienta de testing para PHP popular y poderosa. Instálalo con `composer` y escribe tests en clases y métodos. Aquí un ejemplillo:

```PHP
<?php
use PHPUnit\Framework\TestCase;

class CalculadoraTest extends TestCase {
    public function testSuma() {
        $calculadora = new Calculadora();
        $this->assertEquals(4, $calculadora->suma(2, 2));
    }
}

class Calculadora {
    public function suma($a, $b) {
        return $a + $b;
    }
}
?>
```
Ejecuta los tests con `./vendor/bin/phpunit`. Si todo está bien, verás algo como:

```
OK (1 test, 1 assertion)
```

## Detalles en Profundidad:

Testing automático existe desde los 60, pero ha cobrado más importancia con métodos como TDD (Test-Driven Development). Alternativas a PHPUnit incluyen PHPSpec, Codeception y Behat. Cada uno tiene su enfoque, como BDD (Behavior-Driven Development) en Behat. Implementa testing integrándolo en tu ciclo de desarrollo y ejecútalo regularmente.

## Ver También:

- [PHPUnit](https://phpunit.de/)
- [Introducción a PHPSpec](https://www.phpspec.net/en/stable/)
- [Codeception para pruebas de aceptación](https://codeception.com/)
- [Behat, otro framework de testing con enfoque BDD](https://docs.behat.org/en/latest/)
