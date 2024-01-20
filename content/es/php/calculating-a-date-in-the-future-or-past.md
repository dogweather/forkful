---
title:                "Cálculo de una fecha en el futuro o pasado"
html_title:           "PHP: Cálculo de una fecha en el futuro o pasado"
simple_title:         "Cálculo de una fecha en el futuro o pasado"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Calcular una fecha futura o pasada en PHP consiste en agregar o restar días, meses, años, etc., a una fecha específica. Los programadores suelen hacerlo para hacer un seguimiento del tiempo de las operaciones y para programar eventos futuros.

## Cómo hacerlo:
```PHP
$fecha = new DateTime('2021-01-01');
$fecha->add(new DateInterval('P30D')); //Añade 30 días
echo $fecha->format('Y-m-d'); // Muestra "2021-01-31"

$fecha = new DateTime('2021-01-31');
$fecha->sub(new DateInterval('P1M')); // Resta 1 mes
echo $fecha->format('Y-m-d'); // Muestra "2020-12-31"
```
Este código mostrará las fechas resultantes de agregar 30 días al 1 de enero de 2021 y de restar un mes al 31 de enero de 2021, respectivamente.

## En Detalles:
La idea de manipular fechas se remonta a los primeros días de la programación. En PHP, antes de la versión 5.2.0, los programadores solían usar funciones como `strtotime()` para calcular fechas pero su uso podía resultar confuso. Desde PHP 5.2.0, la clase `DateTime` proporciona una forma más intuitiva y orientada a objetos para trabajar con fechas.

Alternativamente, puedes usar la función `date_modify()` para alterar la fecha, pero `DateTime` y `DateInterval` son generalmente más legibles y seguros.

Al calcular fechas futuras o pasadas, es importante recordar los meses que tienen menos de 31 días y los años bisiestos. Pero la buena noticia es que tanto `DateInterval` como `date_modify()` tienen en cuenta estas variaciones por ti.

## Ver También:
1. Documentación oficial de PHP para la clase DateTime: https://www.php.net/manual/es/class.datetime.php
2. Documentación oficial de PHP para la clase DateInterval: https://www.php.net/manual/es/class.dateinterval.php
3. Un tutorial útil en manejo de fechas y tiempos en PHP: https://www.w3schools.com/php/php_ref_date.asp