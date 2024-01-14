---
title:    "PHP: Convirtiendo una fecha en una cadena"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué

Muchas veces, como programadores, nos encontramos en la necesidad de convertir fechas en cadenas de texto para poder mostrarlas de forma legible al usuario o guardarlas en una base de datos. Esto puede ser especialmente útil en aplicaciones web o sistemas de reserva y gestión de citas. Afortunadamente, en PHP contamos con diversas herramientas que nos permiten realizar esta conversión de manera sencilla.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en PHP, primero debemos utilizar la función `date_format()` que nos permite dar formato a cualquier objeto de tipo `DateTime` y obtener como resultado una cadena de texto. Veamos un ejemplo:

```PHP
$fecha = new DateTime('2021-10-05 13:00:00');
echo date_format($fecha, 'd/m/Y H:i:s');
```

En este caso, estamos creando un objeto `DateTime` con la fecha y hora indicadas y luego utilizamos la función `date_format()` para darle formato a la fecha. Los formatos de fecha y hora en PHP siguen un patrón específico, en este caso hemos utilizado `d` para el día en formato numérico, `m` para el mes en formato numérico, `Y` para el año en formato numérico de 4 dígitos, `H` para la hora en formato de 24 horas, `i` para los minutos y `s` para los segundos. El resultado de este código será `05/10/2021 13:00:00`.

Además, podemos utilizar la función `setLocale()` para establecer el idioma en el que queremos que se muestre la fecha, lo que nos permitirá mostrarla en el formato que sea más común para nuestros usuarios. Por ejemplo:

```PHP
setLocale(LC_ALL, 'es_ES');
echo date_format($fecha, 'l, d \d\e F \d\e\l Y');
```

En este caso, además de dar formato a la fecha, hemos establecido la localización en español y hemos utilizado el formato `l` para el día de la semana en formato de texto completo, `F` para el mes en formato de texto completo y `Y` para el año en formato numérico de 4 dígitos. El resultado de este código será `martes, 05 de octubre del 2021`.

## Profundizando

Además de la función `date_format()`, PHP también cuenta con otras funciones útiles para trabajar con fechas y cadenas de texto. Por ejemplo, la función `date()` nos permite obtener la fecha actual en un formato específico sin necesidad de crear un objeto `DateTime`. También podemos utilizar la función `strtotime()` para convertir una cadena de texto en una fecha en formato `DateTime`, lo que puede ser de gran ayuda cuando recibimos una fecha en formato texto desde un formulario o base de datos.

Otra función interesante es `strptime()`, que nos permite, al igual que `date_format()`, dar formato a un objeto `DateTime`, pero a partir de una cadena de texto en vez de un objeto de tipo `DateTime`.

## Ver también

- [Documentación oficial de PHP sobre el manejo de fechas y tiempos](https://www.php.net/manual/es/datetime.format.php)
- [Ejemplos de formatos de fecha en PHP](https://www.w3schools.com/php/func_date_date_format.asp)
- [Uso de setLocale() en PHP](https://www.php.net/manual/es/function.setlocale.php)