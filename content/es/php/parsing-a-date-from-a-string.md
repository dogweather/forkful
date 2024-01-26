---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:37:51.547939-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Parsear una fecha desde una cadena de texto permite convertirla en un formato que PHP entiende y puede manejar. Los programadores lo hacen para manipular fechas - comparar, almacenar o cambiar su formato - de manera sencilla y efectiva.

## Cómo hacerlo:
Para convertir una cadena a una fecha, PHP tiene una función súper útil: `strtotime()`. Esto intenta convertir cualquier texto que parezca una fecha en un timestamp de Unix, el cual puedes usar para crear una fecha.

```PHP
<?php
$fechaComoCadena = "21-03-2023";
$timestamp = strtotime($fechaComoCadena);
echo date("d/m/Y", $timestamp); // Muestra "21/03/2023"
?>
```

Esta función es muy versátil. Puedes emplearla con diferentes formatos:

```PHP
<?php
$fechaTexto = "next Thursday";
$fecha = strtotime($fechaTexto);
echo date("Y-m-d", $fecha); // Muestra la fecha del próximo jueves en formato Y-m-d
?>
```

También hay un objeto `DateTime` para quienes prefieren la orientación a objetos:

```PHP
<?php
$fechaComoCadena = "2023-03-21 10:00:00";
$fecha = new DateTime($fechaComoCadena);
echo $fecha->format("d-m-Y H:i:s"); // Muestra "21-03-2023 10:00:00"
?>
```

## Profundizando:
Antes, en PHP, todo giraba en torno a `strtotime()` y las funciones `date()` y `time()`. Pero las cosas evolucionaron y surgió la clase `DateTime` en PHP 5.2.0. 

La ventaja de `DateTime` es que maneja mejor los husos horarios y la inmutabilidad (con `DateTimeImmutable`). Algunos prefieren `DateTime` porque es más expresivo y orientado a objetos, mientras que `strtotime()` es funcional y rápido para scripts pequeños.

Otra alternativa más reciente es la clase `IntlDateFormatter` que permite incluso más control sobre la localización y el formato de las fechas.

Implementar esto en PHP es generalmente seguro y predecible porque las funciones suelen adherirse a los estándares de fecha y hora de Unix. Aún así, debes tener cuidado con las zonas horarias y particularmente con cambios en el horario de verano.

## Ver También:
- [Documentación oficial de PHP para `strtotime`](https://www.php.net/manual/es/function.strtotime.php)
- [Documentación oficial de PHP para `DateTime`](https://www.php.net/manual/es/class.datetime.php)
- [Documentación oficial de PHP sobre la clase `IntlDateFormatter`](https://www.php.net/manual/es/class.intldateformatter.php)
- [Funciones de fecha y hora en PHP](https://www.php.net/manual/es/book.datetime.php)
