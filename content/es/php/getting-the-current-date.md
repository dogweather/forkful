---
title:                "PHP: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado cómo obtiene tu teléfono inteligente la fecha actual? ¿O cómo los sitios web muestran la fecha y hora exactas en diferentes zonas horarias? La respuesta es: ¡gracias a la programación en PHP! En este artículo, te mostraré cómo obtener la fecha actual utilizando el lenguaje de programación PHP.

## Cómo hacerlo
Para obtener la fecha actual en PHP, podemos usar la función `date()`. Veamos un ejemplo sencillo:

```PHP
<?php
echo date("Y-m-d"); // mostrará la fecha actual en formato año-mes-día
?>
```
El resultado de este código sería algo como "2021-10-12", dependiendo de la fecha actual. A través de la función `date()`, podemos especificar el formato en el que queremos mostrar la fecha y también podemos añadir la hora, minutos y segundos.

Otro ejemplo interesante es utilizar la función `strftime()` para mostrar la fecha en diferentes idiomas. Por ejemplo, si queremos que la fecha se muestre en español, podemos escribir:

```PHP
<?php
setlocale(LC_ALL, 'es_ES');
echo strftime("%A, %d de %B de %Y"); // mostrará la fecha actual en formato "día de la semana, día de mes de año"
?>
```

El resultado sería "martes, 12 de octubre de 2021".

## Profundizando
Ahora, profundicemos en la función `date()` y en cómo podemos utilizarla para obtener diferentes formatos de fecha y hora. Aquí hay una tabla con algunos de los formatos más comunes:

| Formato | Salida |
|---------|--------|
| Y-m-d | 2021-10-12 |
| m/d/y | 10/12/21 |
| j M Y | 12 Oct 2021 |
| l, j F Y | martes, 12 de octubre de 2021 |

Para obtener una lista completa de los formatos disponibles, puedes consultar la documentación oficial de PHP.

Además, también podemos utilizar la función `strtotime()` para calcular fechas en el pasado o en el futuro. Por ejemplo, si queremos obtener la fecha actual en la que se cumpla un año a partir de hoy, podemos escribir:

```PHP
<?php
echo date("d-m-Y", strtotime("+1 year")); // mostrará la fecha actual en formato día-mes-año con un año añadido
?>
```

## Ver también
- Documentación oficial de PHP sobre funciones de fecha y hora: https://www.php.net/manual/es/ref.datetime.php
- Tutorial en español sobre el manejo de fechas en PHP: https://www.webempresa.com/blog/trabajar-con-datos-de-fecha-y-hora-en-php.html
- Ejemplos de la función `date()` y sus formatos: https://www.php.net/manual/es/function.date.php