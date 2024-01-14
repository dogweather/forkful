---
title:    "PHP: Cambiando a mayúsculas una cadena"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Es común que en la programación, ya sea en PHP u otros lenguajes, nos enfrentemos a la necesidad de manipular cadenas de texto. Una de las tareas más comunes es capitalizar una cadena, es decir, convertir todas las primeras letras de cada palabra en mayúsculas. En este artículo exploraremos cómo hacer esto en PHP y por qué es útil.

## Cómo hacerlo
En PHP, podemos capitalizar una cadena usando la función `ucwords()`. Esta función recibe como argumento una cadena y devuelve esa misma cadena con todas las primeras letras de cada palabra en mayúsculas. Veamos un ejemplo:

```PHP
$cadena = "¡hola mundo!";
echo ucwords($cadena);
```

El resultado de este código sería: ¡Hola Mundo! Como se puede ver, la primera letra de cada palabra ha sido convertida en mayúscula. Además, `ucwords()` también respeta los caracteres acentuados y caracteres especiales.

## Deep Dive
Detrás de la función `ucwords()`, se encuentra una función más poderosa llamada `ucfirst()`. Esta función también capitaliza una cadena, pero solo convierte la primera letra en mayúscula. Esto puede ser útil si solo queremos convertir la primera letra de una palabra en mayúscula. Veamos un ejemplo:

```PHP
$cadena = "quién soy";
echo ucfirst($cadena);
```

La salida de este código sería: Quién soy. Como se puede observar, solo la primera letra de la primera palabra ha sido capitalizada.

## Ver También
* [Documentación oficial de PHP: strtoupper()](https://www.php.net/manual/es/function.ucwords.php)
* [Ejemplos de uso de ucwords()](https://www.php.net/manual/es/function.ucwords.php#104191)
* [Conversión de cadenas con caracteres especiales en PHP](https://www.php.net/manual/es/ref.mbstring.php)