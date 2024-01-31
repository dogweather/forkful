---
title:                "Redondeo de números"
date:                  2024-01-26T03:45:54.905166-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/rounding-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Redondear números significa eliminar los decimales hasta una precisión determinada, a menudo hasta números enteros. Los programadores redondean para simplificar cálculos, mejorar el rendimiento o hacer que las salidas sean amigables para el usuario.

## Cómo hacerlo:
PHP ofrece varias maneras de redondear números: `round()`, `ceil()` y `floor()`. Así es como funcionan:

```php
echo round(3.14159);   // Devuelve 3
echo round(3.14159, 2); // Devuelve 3.14

echo ceil(3.14159);    // Devuelve 4, siempre redondea hacia arriba

echo floor(3.14159);   // Devuelve 3, siempre redondea hacia abajo
```

## Estudio Detallado
Redondear números ha sido esencial en matemáticas y computación desde tiempos antiguos para tratar con decimales infinitos imprácticos. En PHP, `round()` puede tomar un parámetro de precisión y modo, afectando su comportamiento - `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, etc., definen cómo se comportará cuando encuentre un escenario de ".5". La precisión es clave en aplicaciones financieras donde el redondeo puede estar legalmente regulado, afectando cómo se implementa `round()` en el código.

Alternativas a las funciones integradas incluyen métodos de redondeo personalizados o funciones BC Math para aritmética de precisión arbitraria, que son útiles para escenarios que necesitan más control o tratan con números muy grandes donde la precisión nativa puede fallar.

## Ver También
Explora más en el manual de PHP:
- [Función `round` de PHP](https://php.net/manual/en/function.round.php)
- [Función `ceil` de PHP](https://php.net/manual/en/function.ceil.php)
- [Función `floor` de PHP](https://php.net/manual/en/function.floor.php)
- [BC Math para aritmética de precisión arbitraria](https://php.net/manual/en/book.bc.php)
