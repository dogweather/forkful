---
title:                "PHP: Calculando una fecha en el futuro o en el pasado"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el pasado o futuro es una tarea común en la programación. Puede ser necesario para realizar tareas como establecer fechas de eventos, enviar recordatorios automáticos o generar informes programados. Aprender a hacerlo en PHP te permitirá agilizar tu trabajo y automatizar tareas repetitivas.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado, podemos utilizar la función `date()` de PHP. Esta función toma dos parámetros, el primero es el formato de salida deseado y el segundo es un timestamp opcional que representa la fecha a la que queremos aplicar el formato. Utilizaremos el timestamp para indicar cuántos segundos queremos añadir o restar a la fecha actual. A continuación, se muestra un ejemplo de cómo calcular la fecha de hoy más 7 días:

```
PHP print "La fecha dentro de 7 días es: ".date("d/m/Y", strtotime("+7 days"));
```

Este código imprimirá: "La fecha dentro de 7 días es: " seguido de la fecha dentro de 7 días en formato dd/mm/aaaa.

Para calcular fechas en el pasado, podemos utilizar el mismo método, pero utilizando valores negativos en el timestamp. A continuación, se muestra un ejemplo de cómo calcular la fecha de hoy menos 1 mes:

```
PHP print "La fecha hace 1 mes era: ".date("d/m/Y", strtotime("-1 month"));
```

Este código imprimirá: "La fecha hace 1 mes era: " seguido de la fecha de un mes atrás en formato dd/mm/aaaa.

## Profundizando

Además de la función `date()`, PHP también tiene otras funciones útiles para manejar fechas. Algunas de ellas son:

- `strtotime()`: Permite convertir una cadena de texto en un timestamp, lo que facilita el cálculo de fechas en el futuro o pasado.
- `mktime()`: Esta función permite crear un timestamp a partir de una fecha específica.
- `date_create()`: Crea un objeto de fecha que luego puede ser formateado con la función `date()`.
- `strtotime()` y `strftime()`: Estas dos funciones son similares a `date()`, pero permiten dar formato a fechas en diferentes idiomas.

Con estos conocimientos, podemos realizar cálculos de fechas de manera rápida y sencilla en nuestros proyectos de PHP.

## Ver también

- [Documentación de PHP sobre fechas](https://www.php.net/manual/es/book.datetime.php)
- [Guía de formatos de fechas en PHP](https://www.php.net/manual/es/function.date.php)
- [Tutorial de fechas en PHP](https://www.w3schools.com/php/php_date.asp)