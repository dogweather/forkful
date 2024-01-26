---
title:                "Calcular una fecha en el futuro o pasado"
date:                  2024-01-20T17:31:53.733960-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Calcular una fecha en el futuro o pasado significa simplemente sumar o restar días, meses o años a una fecha dada. Los programadores lo hacen para manejar reservas, recordatorios, suscripciones, y cualquier otra funcionalidad que implica el seguimiento del tiempo.

## Cómo Hacerlo:
Para calcular una fecha en el futuro o pasado, PHP ofrece una clase potente: `DateTime`. Vamos a ver cómo se utiliza:

```PHP
<?php
// Fecha actual
$fecha = new DateTime();

// Añadir 10 días
$fecha->modify('+10 days');
echo $fecha->format('Y-m-d') . PHP_EOL;

// Restar 1 mes
$fecha->modify('-1 month');
echo $fecha->format('Y-m-d') . PHP_EOL;

// Calcular fecha para el próximo viernes
$fecha->modify('next Friday');
echo $fecha->format('Y-m-d') . PHP_EOL;
?>
```

Si ejecutas este código:

- La primera salida será la fecha de hoy más 10 días.
- La segunda será la fecha resultante menos un mes.
- La tercera será la fecha del próximo viernes a partir de la fecha resultante.

## Inmersión Profunda:
Históricamente, en PHP se manipulaban las fechas con la función `strtotime` y se formateaban con `date`. Sin embargo, desde la versión 5.2, PHP introdujo la clase `DateTime`, que proporciona métodos más intuitivos para trabajar con fechas. 

Hay alternativas a `DateTime`, como `DateInterval` y `DatePeriod`, para tareas más complejas. Así, `DateTime` sigue siendo la solución más simple para cálculos directos de fechas.

Cuando se trabaja con `DateTime`, hay que tener en cuenta las zonas horarias. PHP utiliza la zona horaria configurada en el servidor por defecto, pero se puede especificar con `setTimezone`.

Otro detalle importante es la inmutabilidad. Desde PHP 5.5, existe `DateTimeImmutable`, que funciona igual que `DateTime`, pero no modifica el objeto original al realizar operaciones, lo que ayuda a prevenir errores en aplicaciones grandes.

## Ver También:

- [Documentación oficial de la clase DateTime](https://www.php.net/manual/es/class.datetime.php)
- [Documentación de DateInterval](https://www.php.net/manual/es/class.dateinterval.php)
- [Documentación de DatePeriod](https://www.php.net/manual/es/class.dateperiod.php)
- [Guía de manejo de zonas horarias en PHP](https://www.php.net/manual/es/timezones.php)
