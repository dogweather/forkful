---
title:                "PHP: Obteniendo la fecha actual"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
Es importante para los programadores poder obtener la fecha y hora actual en sus aplicaciones PHP. Esto les permite realizar tareas específicas, como registrar la hora en que se realizó una acción o mostrar la fecha actual en un formato específico para el usuario.

## Cómo hacerlo
En PHP, podemos obtener la fecha y hora actual utilizando la función `date()`. Esta función acepta dos parámetros: el formato de fecha deseado y un timestamp opcional. Si no se proporciona un timestamp, la función devolverá la fecha y hora actuales.

```PHP
<?php
$fecha_actual = date('d-m-Y H:i:s');
echo $fecha_actual;
```

Este código devolverá la siguiente salida:

`15-07-2021 14:30:29`

Si deseamos obtener la fecha y hora en un formato específico, podemos utilizar los siguientes caracteres en el primer parámetro de la función `date()` para definir el formato deseado:

- `d`: día del mes, con ceros iniciales (01-31)
- `m`: mes, con ceros iniciales (01-12)
- `Y`: año con cuatro dígitos (ej. 2021)
- `H`: hora en formato de 24 horas (00-23)
- `i`: minutos con ceros iniciales (00-59)
- `s`: segundos con ceros iniciales (00-59)

Por ejemplo, si queremos mostrar la fecha y hora en formato español, podemos usar el siguiente código:

```PHP
<?php
setlocale(LC_ALL,'es_ES');
$fecha_actual = date('d \d\e F \d\e Y \a \l\a\s H:i:s');
echo $fecha_actual;
```

Y la salida sería:

`15 de julio de 2021 a las 14:30:29`

## Profundizando
Además de la función `date()`, también podemos utilizar la clase `DateTime` para trabajar con fechas y horas. Esta clase proporciona métodos útiles para formatear, comparar y manipular fechas y horas. Por ejemplo, podemos obtener la fecha y hora actual con la clase `DateTime` de la siguiente manera:

```PHP
<?php
$fecha_actual = new DateTime();
echo $fecha_actual->format('d-m-Y H:i:s');
```

La clase `DateTime` también nos permite realizar operaciones con fechas, como agregar días o horas a una fecha determinada. Por ejemplo, si queremos obtener la fecha y hora en una semana, podemos usar el siguiente código:

```PHP
<?php
$fecha_actual = new DateTime();
$fecha_actual->modify('+1 week');
echo $fecha_actual->format('d-m-Y H:i:s');
```

Y la salida sería:

`22-07-2021 14:30:29`

## Ver también
- [La función `date()` en la documentación de PHP](https://www.php.net/manual/es/function.date.php)
- [La clase `DateTime` en la documentación de PHP](https://www.php.net/manual/es/class.datetime.php)