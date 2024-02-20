---
date: 2024-01-26 04:43:24.939807-07:00
description: "Los n\xFAmeros complejos tienen una parte real y una parte imaginaria,\
  \ usualmente escritos como `a + bi`. Son cruciales en matem\xE1ticas avanzadas,\
  \ f\xEDsica,\u2026"
lastmod: 2024-02-19 22:05:17.662008
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos tienen una parte real y una parte imaginaria, usualmente\
  \ escritos como `a + bi`. Son cruciales en matem\xE1ticas avanzadas, f\xEDsica,\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Los números complejos tienen una parte real y una parte imaginaria, usualmente escritos como `a + bi`. Son cruciales en matemáticas avanzadas, física, ingeniería y ciertos algoritmos de computación. Los programadores trabajan con ellos para manejar cálculos que involucran raíces cuadradas de números negativos y funciones oscilantes.

## Cómo:
PHP proporciona soporte incorporado para números complejos usando la extensión `ext-intl` con la clase `NumberFormatter`. Aquí hay un ejemplo:

```php
// Asegurarse de que la extensión intl está cargada
if (!extension_loaded('intl')) {
    die("La extensión intl no está habilitada. Por favor, habilítala para ejecutar este código.");
}

function addComplexNumbers($a, $b) {
    // Usar NumberFormatter para analizar y formatear números complejos
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Analizar números complejos desde cadenas
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Realizar la adición
    $sum = $numA + $numB;

    // Formatear el resultado como un número complejo
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Salida: 7+10i
```

## Estudio Profundo
Antes de `ext-intl`, PHP no tenía soporte nativo para números complejos. Los desarrolladores usaban funciones o bibliotecas de clases personalizadas para manejar números complejos. Las operaciones complejas podían ser tediosas y propensas a errores, pero `ext-intl` proporciona una manera internacionalizada de presentar y analizar números complejos alineada con la biblioteca ICU.

Sin embargo, para operaciones matemáticas de gran peso, algunos podrían usar bibliotecas externas escritas en lenguajes más amigables con las matemáticas (como C o Python) e interactuar con ellas a través de PHP. En cuanto a la implementación, `ext-intl` maneja todo detrás de escena, asegurando una aritmética precisa mientras abstrae la complejidad del desarrollador.

Históricamente, los números complejos eran mal vistos al ser denominados 'imaginarios', pero desde entonces se han vuelto fundamentales en varios campos científicos y matemáticos, revelando más sobre su significado en el mundo real de lo que su estatus imaginario sugería alguna vez.

## Ver también
- [Manual de PHP sobre NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia sobre números complejos](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: La Manera Correcta - Trabajando con Tipos de Datos](https://phptherightway.com/#data_types)
