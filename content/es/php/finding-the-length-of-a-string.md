---
title:                "PHP: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
La manipulación de cadenas de texto es una parte fundamental en la programación y una de las tareas más comunes es la de encontrar la longitud de una cadena. Saber cómo hacerlo es esencial para cualquier programador.

## Cómo hacerlo
Para encontrar la longitud de una cadena en PHP, podemos utilizar la función ```strlen()```. Esta función toma como argumento una cadena y devuelve el número de caracteres que contiene. Por ejemplo:

```PHP
$cadena = "Hola Mundo";
echo strlen($cadena);
```

Este código imprimirá "10" en la pantalla ya que "Hola Mundo" contiene 10 caracteres.

## Profundizando
Además de la función ```strlen()```, existen otras formas de encontrar la longitud de una cadena en PHP. Por ejemplo, también podemos utilizar la función ```mb_strlen()``` si estamos trabajando con cadenas de texto en diferentes idiomas. Esta función tiene en cuenta la codificación de caracteres utilizada en la cadena para obtener una longitud precisa.

También podemos acceder a la longitud de una cadena usando su índice, ya que en PHP las cadenas son tratadas como arrays de caracteres. Por ejemplo:

```PHP
$cadena = "Hola Mundo";
echo $cadena[5];
```

Este código imprimirá "M" en la pantalla, ya que la "M" se encuentra en el índice 5 de la cadena.

Otra forma de obtener la longitud de una cadena es mediante el uso de la función ```preg_match_all()```. Esta función toma como argumento una expresión regular y una cadena, y devuelve el número de coincidencias encontradas en la cadena. Si le pasamos una expresión regular que busque todos los caracteres (.*) en una cadena, obtendremos como resultado su longitud.

## Ver también
- [strlen() en PHP.net](https://www.php.net/manual/es/function.strlen.php)
- [mb_strlen() en PHP.net](https://www.php.net/manual/es/function.mb-strlen.php)
- [preg_match_all() en PHP.net](https://www.php.net/manual/es/function.preg-match-all.php)