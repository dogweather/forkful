---
title:                "PHP: Convirtiendo una fecha en una cadena de caracteres"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado por qué es importante convertir una fecha en una cadena de caracteres en PHP? Aunque pueda parecer una tarea sencilla, en realidad hay muchas razones por las que puede ser una práctica útil. Por ejemplo, puede ser necesario para mostrar la fecha en un formato específico o para almacenarla en una base de datos. Sea cual sea la razón, en este artículo te mostraremos cómo realizar esta conversión.

## Cómo hacerlo

Para convertir una fecha en una cadena de caracteres, podemos utilizar la función `date()` en PHP. Esta función acepta dos parámetros: el formato de la fecha y la marca de tiempo (timestamp) de la fecha que queremos convertir.

```PHP
// Establecemos la fecha a convertir
$fecha = '2021-08-20';

// Convertimos la fecha al formato "día de la semana, día de mes de año"
$fecha_convertida = date('l, j \d\e F \d\e Y', strtotime($fecha));

// Imprimimos la fecha
echo $fecha_convertida;  // Output: Viernes, 20 de agosto de 2021
```

En este ejemplo, utilizamos el parámetro `l` para obtener el día de la semana, `j` para obtener el día del mes, `F` para obtener el mes y `Y` para obtener el año en formato numérico completo. También hemos utilizado la función `strtotime()` para convertir la fecha a una marca de tiempo.

Puedes experimentar con diferentes formatos y marcas de tiempo para obtener la salida deseada. Aquí tienes una tabla con algunos de los formatos más comunes que puedes utilizar:

| Código | Descripción                    | Output                          |
| ------ | ------------------------------ | ------------------------------- |
| d      | Día del mes en formato numérico | 01-31                           |
| j      | Día del mes sin ceros a la izquierda | 1-31                        |
| l      | Día de la semana                | Domingo-Sábado                  |
| w      | Día de la semana en formato numérico | 0-6                             |
| F      | Mes completo                    | Enero-Diciembre                 |
| m      | Mes en formato numérico          | 01-12                           |
| n      | Mes en formato numérico sin ceros a la izquierda | 1-12                        |
| Y      | Año en formato numérico completo | 2021                            |
| y      | Año en formato numérico          | 21                              |
| H      | Hora en formato 24 horas        | 00-23                           |
| h      | Hora en formato 12 horas        | 01-12                           |
| i      | Minutos                         | 00-59                           |
| s      | Segundos                        | 00-59                           |

## Profundizando

Además de los formatos mencionados antes, `date()` también acepta otros parámetros para obtener información adicional sobre la fecha, como por ejemplo, la diferencia entre la hora actual y la hora especificada. Esto puede ser útil si necesitamos obtener la cantidad de años, meses, días, horas, etc. entre dos fechas.

De manera similar, PHP también ofrece la función `strtotime()` para convertir una cadena de caracteres en una marca de tiempo. Esto puede ser útil si necesitamos ingresar una fecha en formato legible para los humanos en una base de datos.

## Ver también

- [Documentación oficial de PHP de la función `date()` (en inglés)](https://www.php.net/manual/en/function.date.php)
- [Documentación oficial de PHP de la función `strtotime()` (en inglés)](https://www.php.net/manual/en/function.strtotime.php)