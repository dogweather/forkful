---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "PHP: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La conversión de una fecha a una cadena de texto es un proceso común en la programación en PHP. Esto se refiere a tomar una fecha almacenada en formato de fecha y hora y convertirla en una cadena de texto con un formato específico. Los programadores realizan esta conversión para mostrar fechas en diferentes formatos, como en un sitio web o en un archivo.

## Cómo hacerlo:

```PHP
<?php

$fecha = date_create("2021-05-28"); // Crear una fecha
echo date_format($fecha, "d/m/Y"); // Convertir la fecha a una cadena de texto en formato día/mes/año
// Output: 28/05/2021
```

```PHP
<?php

$timestamp = strtotime('next Tuesday'); // Crear un tiempo en formato de timestamp
echo date('d/m/Y', $timestamp); // Convertir el timestamp a una cadena de texto en formato día/mes/año
// Output: 01/06/2021
```

## Investigación en profundidad:

- Contexto histórico: La conversión de una fecha a una cadena de texto se ha vuelto aún más importante con el aumento del uso de la web y la necesidad de mostrar fechas en diferentes idiomas y formatos.
- Alternativas: Además de la función `date_format()`, también se pueden usar otras funciones como `strftime()` y `gmstrftime()` para obtener formatos diferentes.
- Detalles de implementación: La función `date_create()` se puede usar para crear un objeto de fecha a partir de una fecha dada, y la función `date()` se puede usar para convertir un timestamp en una cadena de texto con el formato deseado.

## Ver también:

- [Date and Time Functions - PHP Manual](https://www.php.net/manual/en/ref.datetime.php)
- [Convertir una fecha a formato de cadena - Tutorial de W3Schools](https://www.w3schools.com/php/func_date_date_format.asp)