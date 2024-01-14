---
title:                "PHP: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces en programación, necesitamos convertir una fecha en una cadena de texto para mostrarla en una interfaz de usuario o almacenarla en una base de datos. Aprender cómo hacer esto puede ser muy beneficioso para cualquier programador PHP.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en PHP, podemos utilizar la función `date()`. Esta función acepta dos parámetros: el formato deseado de salida y la fecha que queremos convertir. Por ejemplo, si queremos obtener la fecha actual en formato de día/mes/año, podemos usar la siguiente línea de código:

```PHP
echo date('d/m/Y', time());
```

La función `time()` nos da la marca de tiempo actual, que es necesaria como segundo parámetro para `date()`. La salida de este ejemplo sería algo como "12/09/2021".

También podemos usar la función `strtotime()` para convertir una cadena de texto en una fecha. Este método es especialmente útil si recibimos la fecha en formato de cadena y necesitamos convertirla en un formato específico. Por ejemplo, si tenemos la fecha "12 de Septiembre de 2021" en una variable y queremos convertirla en formato de mes/día/año, podemos hacerlo de la siguiente manera:

```PHP
$fecha = "12 de Septiembre de 2021";
echo date('m/d/Y', strtotime($fecha));
```

La salida sería "09/12/2021".

## Deep Dive

La función `date()` acepta varios caracteres como parámetros para definir el formato de salida. Aquí algunos de los más comunes:

- d: día del mes con dos dígitos
- m: mes con dos dígitos
- Y: año con cuatro dígitos
- l: día de la semana completo (en inglés)
- F: mes completo (en inglés)
- M: mes abreviado (en inglés)
- a: am o pm en minúsculas
- A: AM o PM en mayúsculas

Hay muchos más caracteres disponibles y puedes consultar la documentación oficial de PHP para ver la lista completa.

## Ver también

- [Documentación oficial de PHP para la función `date()`](https://www.php.net/manual/es/function.date.php)
- [Documentación oficial de PHP para la función `strtotime()`](https://www.php.net/manual/es/function.strtotime.php)
- [Listado de caracteres para `date()` en la documentación oficial de PHP](https://www.php.net/manual/es/datetime.format.php)