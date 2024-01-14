---
title:    "PHP: Obteniendo la fecha actual"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué

La obtención de la fecha y hora actuales es una tarea muy común en la programación web. Puede ser necesario para mostrar la fecha de publicación de un artículo, registrar la hora a la que se realizó una acción o simplemente para mostrar la hora local en un reloj en línea. En esta publicación, aprenderemos cómo obtener la fecha y la hora actual en PHP.

## Cómo hacerlo

Para obtener la fecha y hora actuales en PHP, podemos utilizar la función `date()`. Esta función toma dos parámetros: un formato y un timestamp opcional. El timestamp es un valor numérico que representa una fecha y hora específica. Si no se proporciona un timestamp, se utilizará la fecha y hora actuales.

```PHP
<?php
$fecha_actual = date("d/m/Y");
$hora_actual = date("H:i:s");

echo "Hoy es " . $fecha_actual . " y son las " . $hora_actual;
```

La salida de este código sería:

```
Hoy es 08/06/2021 y son las 15:25:32
```

También podemos obtener la fecha y hora en diferentes formatos, por ejemplo:

```PHP
<?php
$fecha_actual = date("l, d F Y");
$hora_actual = date("g:i a");

echo "Hoy es " . $fecha_actual . " y son las " . $hora_actual;
```

La salida sería:

```
Hoy es martes, 08 junio 2021 y son las 3:25 pm
```

Además de `date()`, también podemos utilizar la clase `DateTime` para obtener la fecha y hora actuales en PHP. Esta clase nos permite trabajar con fechas y horas de forma más precisa y nos brinda más opciones de formato.

```PHP
<?php
$fecha_actual = new DateTime();
$hora_actual = $fecha_actual->format('d/m/Y H:i:s');

echo "Hoy es " . $fecha_actual . " y son las " . $hora_actual;
```

La salida sería la misma que en el primer ejemplo.

## Profundizando

Más allá de la función `date()` y la clase `DateTime`, PHP ofrece muchas otras funciones y métodos para trabajar con fechas y horas. Algunas de ellas son:

- `time()`: devuelve el timestamp actual.
- `strtotime()`: convierte una fecha en formato texto en un timestamp.
- `date_default_timezone_set()`: establece la zona horaria predeterminada del sistema.
- `DateTime::setTimeZone()`: establece la zona horaria en un objeto DateTime.

También podemos realizar operaciones matemáticas con fechas, como sumar o restar días, meses o años. La documentación oficial de PHP tiene una lista completa de todas las funciones disponibles para trabajar con fechas y horas.

¡Ahora estás listo para trabajar con fechas y horas en tus proyectos de PHP!

## Ver también

- [Documentación de PHP sobre fechas y horas](https://www.php.net/manual/es/book.datetime.php)
- [Lista de zonas horarias soportadas en PHP](https://www.php.net/manual/es/timezones.php)