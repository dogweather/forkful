---
title:                "Convirtiendo una fecha en una cadena de caracteres"
html_title:           "PHP: Convirtiendo una fecha en una cadena de caracteres"
simple_title:         "Convirtiendo una fecha en una cadena de caracteres"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo es necesario trabajar con fechas y hora. Para presentar esta información al usuario, es importante poder convertir una fecha en un formato de texto legible. En este artículo, aprenderemos cómo convertir una fecha en una cadena utilizando PHP.

## Cómo

Para convertir una fecha a una cadena en PHP, podemos utilizar la función `date()`. Esta función acepta dos argumentos: el formato en el que queremos mostrar la fecha y la fecha en sí. Veamos un ejemplo:

```PHP
$date = date('d-m-Y', strtotime('2021-01-15'));
echo $date; // Salida: 15-01-2021
```

En este ejemplo, utilizamos el formato 'd-m-Y' para mostrar la fecha como día-mes-año. También podríamos haber utilizado otros formatos como 'Y/m/d' o 'j M Y'. La función `strtotime()` convierte la fecha en un formato reconocible por PHP.

A veces, también puede ser necesario obtener la hora junto con la fecha. En ese caso, podemos utilizar el formato 'd-m-Y H:i:s' que agregará la hora, minutos y segundos a la cadena.

```PHP
$datetime = date('d-m-Y H:i:s', strtotime('2021-01-15 10:30:00'));
echo $datetime; // Salida: 15-01-2021 10:30:00
```

## Deep Dive

La función `date()` también puede ser útil para mostrar la fecha y hora actual en nuestro sitio web. Podemos utilizar la palabra clave 'now' en lugar de una fecha para obtener la fecha y hora actuales.

```PHP
$current_datetime = date('d-m-Y H:i:s', strtotime('now'));
echo $current_datetime; // Salida: fecha y hora actuales
```

También podemos usar la función `time()` en lugar de una fecha específica. Esto devolverá un sello de tiempo Unix que representa la fecha y hora actuales.

```PHP
$current_timestamp = date('d-m-Y H:i:s', time());
echo $current_timestamp; // Salida: fecha y hora actuales en formato de sello de tiempo Unix
```

Otra cosa importante a tener en cuenta es que la función `date()` utiliza la zona horaria establecida en la configuración de PHP. Si necesitamos utilizar una zona horaria diferente, podemos usar la función `date_default_timezone_set()` para establecerla.

```PHP
// Establecer la zona horaria a Nueva York
date_default_timezone_set('America/New_York');

// Mostrar la fecha y hora en la zona horaria de Nueva York
$current_datetime = date('d-m-Y H:i:s', strtotime('now'));
echo $current_datetime; // Salida: fecha y hora actuales en zona horaria de Nueva York
```

Por último, es importante tener en cuenta que la función `date()` devuelve una cadena en lugar de un objeto de fecha. Por lo tanto, si necesitamos realizar operaciones matemáticas o comparaciones con fechas, primero debemos convertir la cadena a un objeto de fecha utilizando la función `strtotime()`.

## See Also

- [Documentación de PHP sobre la función date()](https://www.php.net/manual/es/function.date.php)
- [Listado de formatos de fecha admitidos en PHP](https://www.php.net/manual/es/function.date.php#date.format)
- [Documentación de PHP sobre la función strtotime()](https://www.php.net/manual/es/function.strtotime.php)