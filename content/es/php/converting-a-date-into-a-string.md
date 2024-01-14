---
title:    "PHP: Convirtiendo una fecha en una cadena."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Muchas veces cuando se trabaja con fechas en un proyecto de programación, es necesario convertir la fecha en una cadena para poder mostrarla o manipularla de una manera específica. Convertir una fecha en una cadena también nos permite personalizar el formato de la fecha que queremos mostrar, como agregar el nombre del mes o mostrar la fecha en diferentes idiomas.

## Cómo hacerlo

Para convertir una fecha en una cadena en PHP, podemos utilizar la función `date()` y especificar el formato de salida que deseamos. Por ejemplo, si queremos mostrar la fecha actual en el formato "día/mes/año", podemos escribir lo siguiente:

```PHP
echo date('d/m/Y');
```

Este código imprimirá la fecha actual en el formato deseado. También podemos personalizar el formato de salida utilizando diferentes letras y símbolos para representar diferentes partes de la fecha y la hora (por ejemplo, "d" para el día, "m" para el mes, etc).

## Profundizando

Además de utilizar la función `date()`, también podemos convertir una fecha en una cadena utilizando la clase `DateTime` de PHP. Esta clase nos permite manipular fechas de una manera más avanzada, como agregar o restar días, meses o años a una fecha determinada.

Un ejemplo de cómo utilizar la clase `DateTime` para convertir una fecha en una cadena sería el siguiente:

```PHP
// Creamos un objeto DateTime con la fecha actual
$fecha = new DateTime();

// Definimos el formato de salida que queremos
$formato = 'd-m-Y';

// Convertimos la fecha en una cadena utilizando la función format()
echo $fecha->format($formato); // Imprime la fecha actual en el formato "d-m-Y"
```

Esta clase también nos permite especificar una zona horaria y obtener fechas de diferentes zonas horarias, lo que puede ser útil en proyectos en los que se trabaje con usuarios de diferentes lugares del mundo.

## Ver también

- [Función `date()` en PHP](https://www.php.net/manual/es/function.date.php)
- [Clase `DateTime` en PHP](https://www.php.net/manual/es/class.datetime.php)