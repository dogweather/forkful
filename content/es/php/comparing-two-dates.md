---
title:                "Comparando dos fechas"
html_title:           "PHP: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo surgen situaciones en las que es necesario comparar dos fechas. Esto puede ser útil en casos como validar una fecha de inicio y una fecha de finalización en un formulario, ordenar eventos por fecha o verificar si una promoción ya ha caducado.

## Cómo hacerlo

En PHP, hay varias formas de comparar dos fechas. Aquí hay tres ejemplos utilizando diferentes funciones: `strtotime()`, `DateTime` y `strtotime()` con `date()`.

### Utilizando `strtotime()`

La función `strtotime()` convierte una cadena de texto con una fecha a un timestamp, que representa la cantidad de segundos desde el 1 de enero de 1970 a la fecha especificada. Podemos utilizar esto para comparar dos fechas de la siguiente manera:

```PHP
$fecha1 = "2020-01-15";
$fecha2 = "2020-02-20";

if (strtotime($fecha1) < strtotime($fecha2)) {
    echo "La fecha 1 es anterior a la fecha 2";
} else {
    echo "La fecha 1 es posterior o igual a la fecha 2";
}
```

El resultado sería "La fecha 1 es anterior a la fecha 2".

### Utilizando `DateTime`

La clase `DateTime` nos permite trabajar con fechas de una manera más orientada a objetos. Podemos crear dos objetos `DateTime` y compararlos utilizando el método `diff()` que devuelve un objeto `DateInterval`.

```PHP
$fecha1 = new DateTime("2020-01-15");
$fecha2 = new DateTime("2020-02-20");

$diferencia = $fecha1->diff($fecha2);

if ($diferencia->invert == 1) {
    echo "La fecha 1 es anterior a la fecha 2";
} else {
    echo "La fecha 1 es posterior o igual a la fecha 2";
}
```

En este caso, el resultado también sería "La fecha 1 es anterior a la fecha 2".

### Utilizando `strtotime()` y `date()`

También podemos utilizar la función `date()` junto con `strtotime()` para convertir las fechas a un mismo formato y poder compararlas fácilmente.

```PHP
$fecha1 = date("d-m-Y", strtotime("2020-01-15"));
$fecha2 = date("d-m-Y", strtotime("2020-02-20"));

if ($fecha1 < $fecha2) {
    echo "La fecha 1 es anterior a la fecha 2";
} else {
    echo "La fecha 1 es posterior o igual a la fecha 2";
}
```

Este código también nos devuelve "La fecha 1 es anterior a la fecha 2".

## Profundizando

En general, cuando comparamos dos fechas, debemos tener en cuenta su formato y asegurarnos de que ambas estén en el mismo antes de realizar la comparación. Además, cabe mencionar que en algunos casos puede resultar necesario utilizar funciones adicionales para manipular las fechas y obtener el resultado deseado. Es importante investigar y familiarizarse con las diferentes opciones de comparación de fechas en PHP para elegir la más adecuada según nuestro contexto.

## Ver también

Para más información sobre manejo de fechas en PHP, puedes revisar los siguientes enlaces:

- [Documentación oficial de PHP sobre fechas y horas](https://www.php.net/manual/es/datetime.examples-arithmetic.php)
- [Comparando fechas en PHP](https://www.w3resource.com/php-exercises/date-time-exercise/php-date-time-exercise-16.php)
- [Realizar cálculos con fechas en PHP](https://www.geeksforgeeks.org/php-date-function-compare-dates/)