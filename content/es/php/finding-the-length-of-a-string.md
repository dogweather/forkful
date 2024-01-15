---
title:                "Encontrar la longitud de una cadena"
html_title:           "PHP: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
La longitud de una cadena es una información esencial en la programación que nos permite realizar diversas operaciones, como validar entradas de usuario o truncar textos demasiado largos. Saber cómo encontrar la longitud de una cadena en PHP te ayudará a ser un programador más versátil y eficiente.

## Cómo hacerlo
Para encontrar la longitud de una cadena en PHP, podemos utilizar la función `strlen()`. Esta función toma como argumento la cadena de texto y devuelve un número entero que representa la cantidad de caracteres en esa cadena.

```PHP
$cadena = "¡Hola mundo!";
echo strlen($cadena); // output: 12
```

Si queremos encontrar la longitud de una cadena con caracteres multibyte (como los acentos en español), podemos utilizar la función `mb_strlen()`. Esta función también toma como argumento la cadena de texto, pero nos devuelve la cantidad de caracteres contando los bytes en lugar de los símbolos.

```PHP
$cadena = "¡Hola mundo!";
echo mb_strlen($cadena); // output: 11
```

También podemos utilizar la función `mb_strlen()` para contar la cantidad de palabras en una cadena, pasando como segundo argumento el encoding utilizado en la cadena. Por ejemplo, si utilizamos la función `mb_strlen($cadena, "UTF-8")` en una cadena con caracteres especiales en español, obtendremos un resultado más preciso.

```PHP
$cadena = "Esto es una prueba.";
echo mb_strlen($cadena, "UTF-8"); // output: 17
```

## Deep Dive
La función `strlen()` cuenta los bytes que forman una cadena, mientras que `mb_strlen()` cuenta los símbolos. Esto puede generar diferencias en los resultados si utilizamos diferentes encodings. Por ejemplo, si utilizamos la función `strlen()` en una cadena con caracteres multibyte, obtendremos una longitud mayor a la real debido a que cuenta cada byte como un símbolo.

Otra cosa importante a tener en cuenta es que la función `mb_strlen()` solo está disponible si se tiene instalada la extensión mbstring en PHP. Si no se encuentra instalada, podemos utilizar la función `iconv_strlen()` para lograr un resultado similar.

## See Also
- [Documentación oficial de PHP para la función strlen()](https://www.php.net/manual/es/function.strlen.php)
- [Documentación oficial de PHP para la función mb_strlen()](https://www.php.net/manual/es/function.mb-strlen.php)
- [Documentación oficial de PHP para la función iconv_strlen()](https://www.php.net/manual/es/function.iconv-strlen.php)