---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Convertir una fecha en una cadena de caracteres en PHP significa convertir un objeto `DateTime` en un formato de cadena legible. Los programadores hacen esto para facilitar la manipulación de fechas y la presentación de las mismas en una variedad de formatos.

## Cómo hacerlo:

Aquí tienes un ejemplo sencillo de cómo convertir un objeto `DateTime` en una cadena.

```PHP
<?php
  $fecha= new DateTime();  // Crea un nuevo objeto DateTime
  echo $fecha->format('Y-m-d H:i:s');  // Convierte la fecha a formato cadena 
?>
```

La salida de este código será la fecha y hora actual en el formato "AAAA-MM-DD HH:MM:SS". Por ejemplo:

```PHP
2022-03-09 14:20:45
```

## Inmersión Profunda

El método `format()` que estamos usando aquí ha sido parte de PHP desde la versión 5.2.0. Permite a los programadores presentar fechas y horas en una variedad de formatos.

Existen alternativas para convertir una fecha en una cadena en PHP, como `date()` y `strftime()`, aunque `DateTime::format` es generalmente preferido por su flexibilidad y orientación a objetos.

Es importante tener en cuenta que `DateTime::format` retorna una cadena en el formato especificado. Sin embargo, no cambia el objeto `DateTime` original. 

## Ver También

Puedes encontrar más información y opciones de formato para el método `format()` en la documentación oficial de PHP aquí: [PHP: DateTime::format - Manual](https://www.php.net/manual/es/datetime.format.php).

Para una descripción más detallada del manejo de fechas y tiempo en PHP, consulta este enlace: [PHP: Dates and Time - Manual](https://www.php.net/manual/es/book.datetime.php).