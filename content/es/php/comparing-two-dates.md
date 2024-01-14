---
title:                "PHP: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en programación?

Comparar dos fechas puede ser una tarea común en programación. Esto se debe a que a menudo es necesario verificar si una fecha es anterior o posterior a otra, o si ambas son iguales. En este artículo, aprenderemos cómo hacerlo en PHP.

## Cómo hacerlo

Para comparar dos fechas en PHP, podemos utilizar la función "strtotime()" para convertir una fecha en un formato de tiempo para su fácil manipulación. A continuación, podemos utilizar la función "date()" para formatear la fecha de acuerdo a nuestras necesidades.

Veamos un ejemplo de cómo comparar dos fechas en PHP:

```PHP
$fecha1 = '2020-05-25';
$fecha2 = '2020-06-01';

// convertir fechas a formato de tiempo
$timestamp1 = strtotime($fecha1);
$timestamp2 = strtotime($fecha2);

// formatear fechas para comparar
$formatted1 = date('Y-m-d', $timestamp1);
$formatted2 = date('Y-m-d', $timestamp2);

// comparar fechas
if($formatted1 > $formatted2) {
    echo "La fecha 1 es posterior a la fecha 2";
} elseif($formatted1 < $formatted2) {
    echo "La fecha 1 es anterior a la fecha 2";
} else {
    echo "Ambas fechas son iguales";
}

// output: La fecha 1 es anterior a la fecha 2
```

En este ejemplo, hemos utilizado la función "strtotime()" para convertir las fechas en formato de tiempo y luego la función "date()" para formatearlas como fechas nuevamente. Finalmente, comparamos las fechas utilizando operadores como ">", "<" y "==".

## Inmersión profunda

Una cosa importante a tener en cuenta al comparar fechas en PHP es asegurarse de que ambas fechas estén en el mismo formato. De lo contrario, puede llevar a resultados inesperados. Además, al utilizar la función "strtotime()", puede ocurrir un comportamiento impredecible al pasar una fecha en formato dd/mm/aaaa debido a diferencias regionales. Es importante asegurarse de que el formato de fecha utilizado sea compatible en diferentes entornos.

## Ver también

- [Documentación de strtotime() en PHP](https://www.php.net/manual/es/function.strtotime.php)
- [Documentación de date() en PHP](https://www.php.net/manual/es/function.date.php)
- [Tutorial de PHP datetime](https://www.w3schools.com/php/php_date.asp)