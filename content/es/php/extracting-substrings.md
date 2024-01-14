---
title:                "PHP: Extrayendo subcadenas"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas o subcadenas de una cadena de texto es una tarea común en la programación. Puede ser útil para manipular datos o para realizar búsquedas específicas en una cadena.

## Cómo

Para extraer una subcadena de una cadena en PHP, podemos utilizar la función `substr()`. Esta función acepta tres parámetros: la cadena original, la posición inicial y la longitud de la subcadena.

Por ejemplo, si tenemos la cadena "Hola mundo" y queremos extraer "mundo", podemos hacerlo de la siguiente manera:

```PHP
$cadena = "Hola mundo";
$subcadena = substr($cadena, 5, 5);
echo $subcadena; // resultado: mundo
```

También podemos utilizar números negativos para obtener la subcadena desde el final de la cadena. Por ejemplo, si queremos extraer "mundo" de la cadena "Hola mundo" utilizando números negativos, podemos hacerlo de la siguiente manera:

```PHP
$cadena = "Hola mundo";
$subcadena = substr($cadena, -5, 5);
echo $subcadena; // resultado: mundo
```

Además, también podemos utilizar la función `strpos()` para obtener la posición de una subcadena en una cadena y luego utilizar la función `substr()` para extraer la subcadena utilizando esa posición.

## Profundizando

Existen muchas otras funciones y métodos en PHP que pueden ser útiles para extraer subcadenas, como por ejemplo `str_replace()`, `preg_match()` y `explode()`. Además, también podemos utilizar expresiones regulares para realizar búsquedas y sustituciones más complejas en una cadena.

En resumen, extraer subcadenas puede ser necesario en muchas situaciones y conocer las diferentes opciones para hacerlo en PHP puede ser muy útil para cualquier programador.

## Ver también

- Documentación de PHP sobre `substr()` (https://www.php.net/manual/es/function.substr.php)
- Documentación de PHP sobre `strpos()` (https://www.php.net/manual/es/function.strpos.php)
- Documentación de PHP sobre expresiones regulares (https://www.php.net/manual/es/reference.pcre.pattern.syntax.php)