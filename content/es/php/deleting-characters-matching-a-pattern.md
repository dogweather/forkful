---
title:                "PHP: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

¡Hola amigos programadores! ¿Alguna vez te has encontrado en una situación en la que necesitas eliminar ciertos caracteres de una cadena de texto? Si es así, ¡este blog es para ti! En esta publicación, exploraremos cómo eliminar caracteres que coincidan con un patrón en PHP.

## ¿Por qué hacerlo?

Eliminar caracteres que coinciden con un patrón puede ser útil en muchas situaciones diferentes. Por ejemplo, si estás trabajando con datos de entrada del usuario, es posible que desees eliminar todos los caracteres que no sean numéricos para asegurarte de que solo recibas números válidos. O puede que necesites eliminar caracteres de un texto para formatearlo de una manera específica. Sea cual sea la razón, ¡aprender a eliminar caracteres que coinciden con un patrón te ayudará a manejar mejor tus datos!

## Cómo hacerlo

En PHP, hay varias formas de eliminar caracteres que coincidan con un patrón. Aquí hay algunos ejemplos con su correspondiente código y salida:

1. Utilizando la función `preg_replace()`: Esta función nos permite reemplazar una cadena de texto que coincida con un patrón con una cadena vacía. Veamos un ejemplo:

```PHP
$cadena = "Hola, 123 mundo!";
echo preg_replace('/[0-9]/', '', $cadena); // Salida: "Hola, mundo!"
```

2. Utilizando la función `filter_var()`: Esta función nos permite filtrar una variable utilizando un filtro especificado. En este caso, utilizaremos el filtro `FILTER_SANITIZE_NUMBER_INT` para eliminar todos los caracteres que no sean numéricos:

```PHP
$cadena = "¡Hola, 123 mundo!";
echo filter_var($cadena, FILTER_SANITIZE_NUMBER_INT); // Salida: 123
```

3. Utilizando la función `str_replace()`: Esta función nos permite reemplazar una cadena de texto con otra. Veamos un ejemplo en el que reemplazamos todos los números por un espacio en blanco:

```PHP
$cadena = "¡Hola, 123 mundo!";
echo str_replace(range(0, 9), " ", $cadena); // Salida: "¡Hola, mundo!"
```

## Profundizando

Ahora que ya sabes cómo eliminar caracteres que coinciden con un patrón en PHP, veamos algunos detalles más técnicos sobre cómo funciona esto.

- La función `preg_replace()` utiliza expresiones regulares para buscar un patrón en una cadena de texto y reemplazarlo con otra cadena.
- La función `filter_var()` utiliza los filtros predefinidos de PHP para realizar una tarea específica. Puedes encontrar más información sobre los filtros disponibles en la [documentación de PHP](https://www.php.net/filter.filters).
- La función `str_replace()` reemplaza todas las instancias de una cadena de texto en otra.

¡Eso es todo! Ahora ya sabes cómo eliminar caracteres que coinciden con un patrón en PHP. ¡Esperamos que te sea útil en tus proyectos futuros!

## Ver también

- [Documentación de PHP sobre la función `preg_replace()`](https://www.php.net/manual/es/function.preg-replace.php)
- [Documentación de PHP sobre la función `filter_var()`](https://www.php.net/manual/es/function.filter-var.php)
- [Documentación de PHP sobre la función `str_replace()`](https://www.php.net/manual/es/function.str-replace.php)