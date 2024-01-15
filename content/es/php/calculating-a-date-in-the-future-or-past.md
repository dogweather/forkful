---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "PHP: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué
¡Hola a todos! Si eres un programador de PHP, probablemente ya tengas algunas habilidades básicas para trabajar con fechas. Pero, ¿alguna vez has necesitado calcular una fecha en el futuro o en el pasado? ¡En este artículo te mostraré cómo hacerlo de manera sencilla! Así que sigue leyendo.

## Cómo hacerlo
Para calcular una fecha en el futuro o en el pasado, primero necesitas conocer la fecha actual (para obtener esta información, puedes utilizar la función `date()` de PHP). A continuación, puedes utilizar la función `strtotime()` para sumar o restar un intervalo de tiempo a esta fecha.

Por ejemplo, si quieres saber qué día será dentro de 7 días, puedes hacerlo de la siguiente manera:

```PHP
$fecha_actual = date("Y-m-d"); // obtiene la fecha actual en el formato "año-mes-día"
$fecha_futura = strtotime("+7 days", $fecha_actual); // suma 7 días a la fecha actual
echo date("Y-m-d", $fecha_futura); // imprime la fecha futura en el mismo formato
```

La salida de este código sería `2021-04-11`, ya que hoy es 4 de abril.

También puedes utilizar `strtotime()` para restar un intervalo de tiempo. Por ejemplo, si quieres saber qué día fue hace 2 semanas, puedes hacerlo de la siguiente manera:

```PHP
$fecha_actual = date("Y-m-d");
$fecha_pasada = strtotime("-2 weeks", $fecha_actual);
echo date("Y-m-d", $fecha_pasada);
```

La salida sería `2021-03-21`.

## Deep Dive
La función `strtotime()` es muy útil para trabajar con fechas en PHP. Puedes utilizar una amplia variedad de formatos de tiempo para especificar el intervalo que deseas sumar o restar. Por ejemplo, en lugar de utilizar `"+7 days"`, también puedes usar `"next week"` o `"next thursday"`, y la función calculará la fecha correspondiente.

Además, si necesitas una fecha específica pero no sabes cómo escribirla en PHP, puedes utilizar la página web [www.php.net](https://www.php.net/manual/es/function.strtotime.php) para obtener la sintaxis correcta.

¡Ahora ya sabes cómo calcular fechas en el futuro o en el pasado en PHP! Otra función relacionada que podría interesarte es `date_add()`, que te permite sumar un intervalo de tiempo a una fecha específica en lugar de la fecha actual. ¡Explora y diviértete con estas funciones!

## Ver también
- [Documentación oficial de PHP sobre strtotime()](https://www.php.net/manual/es/function.strtotime.php)
- [Más ejemplos de cálculos de fechas en PHP](https://www.ite.educacion.es/formacion/materiales/131/cd/php/fecha.html#sumaromostrar)