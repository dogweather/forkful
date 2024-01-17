---
title:                "Encontrando la longitud de una cadena"
html_title:           "PHP: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

¿Qué & Por qué?

La longitud de una cadena es la cantidad de caracteres que componen esa cadena. Los programadores suelen encontrar la longitud de una cadena para realizar una variedad de tareas, como validar datos de entrada, formatear texto o realizar operaciones matemáticas.

Cómo:

Determinar la longitud de una cadena en PHP es sencillo. Simplemente utilizamos la función `strlen()` seguida de la cadena entre paréntesis. Por ejemplo:

```PHP
$cadena = "¡Hola mundo!";
echo strlen($cadena);
```

Esto imprimirá "12", ya que la cadena contiene 12 caracteres. Podemos utilizar esta función en cualquier tipo de cadena, ya sea una variable, una cadena de texto directa o incluso una cadena que contenga caracteres especiales.

Estado de Animo

La función `strlen()` existe en PHP desde sus primeras versiones y se utiliza ampliamente en la programación web. Sin embargo, también existen otras formas de obtener la longitud de una cadena en PHP, como la función `mb_strlen()` que tiene en cuenta la codificación de caracteres y puede ser útil en proyectos multilingües.

Además, es importante tener en cuenta que la longitud de una cadena puede variar dependiendo del tipo de codificación utilizado. Por ejemplo, una cadena en UTF-8 puede tener una longitud diferente a una cadena en ASCII, ya que los caracteres en UTF-8 ocupan más espacio.

Ver También:

- Documentación oficial de PHP sobre la función `strlen()`: https://www.php.net/manual/es/function.strlen.php
- Documentación oficial de PHP sobre la función `mb_strlen()`: https://www.php.net/manual/es/function.mb-strlen.php