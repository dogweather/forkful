---
title:                "Buscando y reemplazando texto"
html_title:           "PHP: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué buscar y reemplazar texto en PHP?

¿Alguna vez te has encontrado en una situación en la que necesitas hacer cambios en una gran cantidad de texto en tu código PHP? Ya sea para corregir errores ortográficos, cambiar nombres de variables o actualizar información, buscar y reemplazar texto es una herramienta útil y eficiente que facilita el proceso de edición en tu código. En este artículo, aprenderás cómo realizar esta tarea común en PHP de manera sencilla y rápida.

## Cómo hacerlo

La función principal que utilizaremos para buscar y reemplazar texto en PHP es `str_replace()`. Esta función toma tres parámetros obligatorios: el texto que queremos buscar, el texto con el que queremos reemplazarlo y el string en el que haremos la búsqueda. Veamos un ejemplo sencillo:

```PHP
$texto = "Hola mundo!";
$nuevo_texto = str_replace("mundo", "amigos", $texto);
echo $nuevo_texto;
```

Este código producirá la salida `Hola amigos!`, ya que hemos buscado el texto "mundo" en la variable `$texto` y lo hemos reemplazado por "amigos".

Pero ¿qué pasa si queremos buscar y reemplazar más de una instancia de un texto en una cadena? En ese caso, podemos utilizar un cuarto parámetro opcional en la función `str_replace()`, que especifica el número máximo de veces que se realizará el reemplazo.

Por ejemplo, si queremos reemplazar solo la primera instancia de "mundo" en `$texto`, podemos agregar un cuarto parámetro con el valor `1`:

```PHP
$texto = "Hola mundo! Hola mundo!";
$nuevo_texto = str_replace("mundo", "amigos", $texto, 1);
echo $nuevo_texto;

//Salida: Hola amigos! Hola mundo!
```

Por último, si quieres ser más específico en tu búsqueda, también puedes utilizar expresiones regulares en la función `str_replace()`. Por ejemplo, si solo quieres reemplazar números en una cadena, puedes hacer lo siguiente:

```PHP
$numeros = "1, 2, 3, 4";
$nueva_cadena = str_replace("/[0-9]/", "-", $numeros);
echo $nueva_cadena;

//Salida: -, -, -, -
```

## Profundizando

La función `str_replace()` es una herramienta poderosa para buscar y reemplazar texto en PHP, pero también hay otras funciones que pueden ser útiles dependiendo de tu situación. Algunas de ellas son `preg_replace()`, `substr_replace()` y `strtr()`, las cuales tienen diferentes funcionalidades y parámetros. Te recomendamos revisar la documentación oficial de PHP para familiarizarte con ellas.

Es importante tener en cuenta que estas funciones realizan búsquedas sensibles a mayúsculas y minúsculas por defecto. Sin embargo, si quieres hacer una búsqueda case-insensitive (ignorando mayúsculas y minúsculas), puedes utilizar la función `str_ireplace()`.

## Véase también

- Documentación oficial de PHP para `str_replace()` - https://www.php.net/manual/es/function.str-replace.php
- Documentación oficial de PHP para expresiones regulares - https://www.php.net/manual/es/pcre.pattern.php
- Tutorial de expresiones regulares en PHP - https://code.tutsplus.com/es/tutorials/using-regular-expressions-in-php--net-2215
- Video tutorial sobre buscar y reemplazar texto en PHP - https://www.youtube.com/watch?v=J8cP_hlwXo4