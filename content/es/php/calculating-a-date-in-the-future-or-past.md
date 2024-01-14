---
title:    "PHP: Calculando una fecha en el futuro o pasado"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez te has preguntado cómo calcular una fecha en el futuro o en el pasado? Quizás necesitas programar una tarea o evento que suceda en una fecha específica, o simplemente estás interesado en aprender nuevas habilidades de programación. Sea cual sea la razón, calcular fechas en PHP puede ser una herramienta útil para tener en tu arsenal de programación.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en PHP, podemos utilizar la función `strtotime()`. Esta función toma dos argumentos: una cadena de texto con la fecha deseada y un segundo argumento opcional que establece una fecha de referencia.

Por ejemplo, si queremos calcular la fecha de mañana, podemos utilizar el siguiente código:

````PHP
<?php
$manana = strtotime("tomorrow");
echo "Mañana es " . date("d/m/Y", $manana);
````

La salida de este código sería:

```
Mañana es 10/04/2020
```

También podemos establecer una fecha de referencia para calcular una fecha en el futuro o en el pasado en relación a esa fecha. Por ejemplo, si queremos calcular la fecha dentro de una semana, podemos utilizar el siguiente código:

````PHP
<?php
$proximaSemana = strtotime("+1 week");
echo "La fecha dentro de una semana es " . date("d/m/Y", $proximaSemana);
````

La salida de este código sería:

```
La fecha dentro de una semana es 17/04/2020
```

También podemos especificar una fecha de referencia utilizando la función `strtotime()` como segundo argumento. Por ejemplo, si queremos calcular la fecha dentro de dos semanas a partir de una fecha específica, podemos utilizar el siguiente código:

````PHP
<?php
$fecha = strtotime("07-04-2020");
$proximasDosSemanas = strtotime("+2 weeks", $fecha);
echo "La fecha dentro de dos semanas es " . date("d/m/Y", $proximasDosSemanas);
````

La salida de este código sería:

```
La fecha dentro de dos semanas es 21/04/2020
```

## Profundizando

La función `strtotime()` también nos permite calcular fechas utilizando formatos de tiempo más complejos, como por ejemplo "1st Thursday next month" (el primer jueves del mes próximo) o "first day of last month" (primer día del mes pasado). Puedes encontrar una lista completa de formatos válidos en la [documentación oficial de PHP](https://www.php.net/manual/es/datetime.formats.php).

Es importante tener en cuenta que la función `strtotime()` utiliza la zona horaria del servidor para calcular las fechas, por lo que es posible que debas especificar la zona horaria deseada utilizando la función `date_default_timezone_set()`. También es posible realizar cálculos entre diferentes zonas horarias utilizando la clase `DateTime` de PHP.

## Ver también

- [Documentación de PHP sobre la función strtotime()](https://www.php.net/manual/es/function.strtotime.php)
- [Documentación de PHP sobre la clase DateTime](https://www.php.net/manual/es/class.datetime.php)
- [Lista de formatos válidos para strtotime()](https://www.php.net/manual/es/datetime.formats.php)