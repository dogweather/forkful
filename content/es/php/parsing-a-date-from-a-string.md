---
title:                "Analizando una fecha de una cadena."
html_title:           "PHP: Analizando una fecha de una cadena."
simple_title:         "Analizando una fecha de una cadena."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Pasar una fecha de una cadena de texto simplemente significa convertir una cadena de caracteres en una fecha reconocible y utilizable en nuestro código. Los programadores suelen hacer esto para poder realizar cálculos, manipular y mostrar fechas en forma legible.

## Cómo hacerlo:
```PHP
$dateStr = "20-12-2020";

// usando la función date_parse_from_format()
$date = date_parse_from_format("d-m-Y", $dateStr);

// salida esperada: 
Array
(
    [year] => 2020
    [month] => 12
    [day] => 20
    [hour] => 
    [minute] => 
    [second] => 
    [fraction] => 
    [warning_count] => 0
    [warnings] => Array
        (
        )

    [error_count] => 0
    [errors] => Array
        (
        )

    [is_localtime] => 
    [zone_type] => 0
)
```
En este ejemplo, usamos la función ```date_parse_from_format()``` para convertir una cadena de texto en un array con información sobre la fecha. Simplemente especificamos el formato en el que está la cadena y la cadena en sí como argumentos de la función.

Otra forma común de hacerlo es usando la función ```strtotime()```, que convierte una cadena de texto en un timestamp (el número de segundos desde la época Unix). Luego, se puede utilizar la función ```date()``` para formatear el timestamp en una fecha legible.

```PHP
$dateStr = "20-12-2020";

// usando la función strtotime()
$date = strtotime($dateStr);

// salida esperada: 1608403200 (timestamp de la fecha indicada)

// formateando el timestamp con la función date()
echo date("d-m-Y", $date); // salida esperada: 20-12-2020
```
En este ejemplo, primero convertimos la cadena de texto en un timestamp y luego lo formateamos con la función ```date()```.

## Inmersión profunda:
En un mundo cada vez más digitalizado, la manipulación de fechas se ha vuelto cada vez más importante para los programadores. La conversión de fechas de cadenas de texto a formatos utilizables por el código fue un gran avance en la programación de computadoras, ya que antes era necesario realizar cálculos manuales para obtener resultados precisos.

Además de las funciones mencionadas anteriormente, hay otros métodos para realizar esta tarea en PHP, como el uso de expresiones regulares o bibliotecas de terceros. También es importante tener en cuenta las diferentes zonas horarias y formatos de fecha utilizados en diferentes regiones del mundo al realizar estas conversiones.

## Ver también:
- [Documentación de PHP sobre la función date_parse_from_format()](https://www.php.net/manual/es/function.date-parse-from-format.php)
- [Documentación de PHP sobre la función strtotime()](https://www.php.net/manual/es/function.strtotime.php)
- [Explicación detallada sobre cómo trabajar con fechas en PHP](https://www.php.net/manual/es/function.date-parse-from-format.php)