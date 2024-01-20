---
title:                "Analizando una fecha desde una cadena de texto"
html_title:           "PHP: Analizando una fecha desde una cadena de texto"
simple_title:         "Analizando una fecha desde una cadena de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué Y Por Qué?
Analizar una fecha desde una cadena es el acto de interpretar la información de fecha y hora a partir de un texto en un formato específico. Los programadores hacen esto para manipular y utilizar estas fechas en sus aplicaciones.

## Cómo Se Hace:
Vamos a mostrar cómo se analiza una fecha con el formato `Y-m-d`, por ejemplo `2022-03-17`. La forma más fácil es usar `strtotime()` y `date()`. Aquí lo tienes en código PHP:

```PHP
<?php
   $strFecha = "2022-03-17";
   $timestamp = strtotime($strFecha);
   echo date("d-m-Y", $timestamp);
?>
```

La salida será:

```
17-03-2022
```

## Inmersión Profunda
Originalmente, en PHP, solo podríamos analizar cadenas de fecha y hora a través de funciones como `strtotime()`. Pero desde PHP 5.2.0, tenemos la clase `DateTime` que proporciona métodos mucho más flexibles y potentes.

Aquí tienes un ejemplo usando la clase `DateTime`:

```PHP
<?php
   $strFecha = "2022-03-17";
   $fecha = DateTime::createFromFormat('Y-m-d', $strFecha);
   echo $fecha->format('d-m-Y');
?>
```

Además de `DateTime` y `strtotime()`, existen otras libreras y métodos como `date_parse()`. Escoger el mejor depende de tu caso de uso específico.

## Ver También
1. [Documentación oficial de PHP para DateTime](https://www.php.net/manual/es/class.datetime.php)
2. [Función strtotime() en PHP](https://www.php.net/manual/es/function.strtotime.php)
3. [Función date_parse() en PHP](https://www.php.net/manual/es/function.date-parse.php) 

Recuerda, la elección de la forma en que analizas fechas desde cadenas dependerá de tus necesidades específicas y de la estructura de tus datos. ¡Así que sigue experimentando!