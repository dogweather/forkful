---
title:    "PHP: Obteniendo la fecha actual"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual es una habilidad importante en programación

Programar puede ser abrumador y abarcar una amplia gama de habilidades. Entre todas las cosas que debes aprender, a veces puede parecer innecesario enfocarse en cosas simples como obtener la fecha actual. Sin embargo, esta es una habilidad importante a tener en tu arsenal de programación ya que puede ser útil en una variedad de escenarios. Ya sea para registrar el tiempo en que se realizó una acción, mostrar la fecha en la que se creó una entrada de blog o simplemente para fines de depuración, saber cómo obtener la fecha actual es una habilidad esencial en PHP.

## Cómo obtener la fecha actual en PHP

La función `date()` es la forma más común de obtener la fecha actual en PHP. Esta función acepta dos parámetros, el primero es el formato de fecha deseado y el segundo es un valor de tiempo opcional. A continuación se muestra un ejemplo de cómo obtener la fecha actual en formato de día, mes y año:

```PHP
<?php
echo date('d/m/Y');
```

Este código producirá un resultado similar al siguiente:

```
05/08/2021
```

También es posible incluir el día de la semana en el formato de fecha, así como cambiar el formato de los separadores de la fecha. Por ejemplo, el siguiente código producirá una fecha con el día de la semana abreviado y usando guiones como separadores:

```PHP
<?php
echo date('D-d/m/Y');
```

Y el resultado sería:

```
Thu-05/08/2021
```

Otra forma de obtener la fecha actual en PHP es utilizando la función `strtotime()`. Esta función convierte una fecha legible por humanos a un número de segundos desde el 1 de enero de 1970. A continuación se muestra un ejemplo de cómo utilizar `strtotime()` junto con `date()` para obtener la fecha de hoy en el formato de día y mes abreviado:

```PHP
<?php
echo date('D, d M Y', strtotime('today'));
```

Este código producirá algo similar a:

```
Thu, 05 Aug 2021
```

## Profundizando en la obtención de la fecha actual en PHP

La función `date()` es muy versátil y permite una gran cantidad de opciones para formatear la fecha. Puedes consultar la documentación oficial de PHP para obtener una lista completa de los formatos disponibles y cómo usarlos.

También es importante tener en cuenta que la función `date()` utiliza la zona horaria del servidor. Por lo tanto, si necesitas obtener la fecha actual en una zona horaria diferente, es necesario establecerla utilizando la función `date_default_timezone_set()` antes de utilizar `date()`.

## Ver también

- [Documentación oficial de PHP sobre la función date()](https://www.php.net/manual/es/function.date.php)
- [Documentación oficial de PHP sobre la función strtotime()](https://www.php.net/manual/es/function.strtotime.php)