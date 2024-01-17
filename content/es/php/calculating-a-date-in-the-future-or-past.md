---
title:                "Calculando una fecha en el futuro o en el pasado."
html_title:           "PHP: Calculando una fecha en el futuro o en el pasado."
simple_title:         "Calculando una fecha en el futuro o en el pasado."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Calcular una fecha en el futuro o pasado es una tarea común dentro de la programación. Esto se refiere a la capacidad de obtener una fecha con cierta cantidad de días, meses o años agregados o restados a una fecha base. Los programadores suelen utilizar esta funcionalidad para manejar tareas como programar eventos, establecer fechas de vencimiento y generar informes basados en ciertos períodos de tiempo.

## Cómo hacerlo:

Para calcular una fecha en el futuro o pasado utilizando PHP, puede utilizar la función `strtotime()` que convierte una cadena de fecha en formato legible por una máquina a un timestamp. Luego, puede usar la función `date()` para dar formato a la fecha deseada. A continuación se muestra un ejemplo de cómo calcular una fecha 30 días a partir de hoy:

```PHP
$fecha_base = date('Y-m-d'); //Obtenemos la fecha actual
$nueva_fecha = date('Y-m-d', strtotime('+30 days', strtotime($fecha_base))); //Agregamos 30 días a la fecha base
echo $nueva_fecha; //Imprimimos la nueva fecha
```
Salida: `2020-12-11`

## Profundizando:

**Contexto histórico:** Antes de la existencia de funciones como `strtotime()` y `date()`, los programadores debían realizar cálculos complejos para obtener fechas en el futuro o pasado, lo que llevaba mucho tiempo y era propenso a errores. La introducción de estas funciones en PHP simplificó en gran manera esta tarea.

**Alternativas:** Además de `strtotime()`, también se puede utilizar la función `mktime()` para crear un timestamp y luego utilizar `date()` para darle formato. Otra alternativa es utilizar la clase `DateTime` que ofrece una variedad de métodos para manipular fechas.

**Detalles de implementación:** Internamente, `strtotime()` utiliza la función de C `strtottime()` para parsear la cadena de fecha y convertirla en un timestamp. Luego, `date()` lo convierte en un formato deseado utilizando la función C `strftime()`.

## Ver también:

- [Función strtotime() en la documentación de PHP](https://www.php.net/manual/es/function.strtotime.php)
- [Función date() en la documentación de PHP](https://www.php.net/manual/es/function.date.php)
- [Clase DateTime en la documentación de PHP](https://www.php.net/manual/es/class.datetime.php)