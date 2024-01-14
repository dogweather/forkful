---
title:    "PHP: Encontrando la longitud de una cadena"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué

A veces en la programación es necesario saber la longitud de una cadena de texto. Esto puede ser útil para realizar operaciones como validar entradas de usuario o truncar un texto demasiado largo. En este artículo, exploraremos cómo encontrar la longitud de una cadena en PHP.

## Cómo hacerlo

En PHP, podemos usar la función `strlen()` para encontrar la longitud de una cadena. Esta función toma como parámetro la cadena de texto y devuelve su longitud como un número entero. Veamos un ejemplo:

```PHP
<?php
$cadena = "¡Hola Mundo!";
$longitud = strlen($cadena);
echo "La longitud de la cadena es: " . $longitud;
?>
```

Este código producirá la siguiente salida:

```
La longitud de la cadena es: 11
```

También podemos usar la función `mb_strlen()` si la cadena contiene caracteres multibyte, como letras acentuadas o emojis. Esta función toma un parámetro adicional opcional para especificar el conjunto de caracteres utilizado en la cadena.

Ahora, si queremos encontrar la longitud de una cadena de texto que incluya etiquetas HTML, debemos usar la función `strip_tags()` para eliminar las etiquetas antes de usar `strlen()` o `mb_strlen()`. De lo contrario, la longitud devuelta incluirá las etiquetas, lo cual no es lo que buscamos.

## Profundizando

Tener en cuenta que `strlen()` y `mb_strlen()` cuentan los espacios en blanco y los caracteres especiales como parte de la longitud de una cadena. Esto puede afectar los cálculos si necesitamos una longitud exacta en ciertos casos. Para solucionar esto, podemos usar la función `trim()` para eliminar los espacios en blanco al inicio y al final de la cadena antes de usar `strlen()` o `mb_strlen()`.

También podemos usar la función `str_replace()` para reemplazar ciertos caracteres antes de contar la longitud de la cadena. Esto puede ser útil en casos donde queremos ignorar ciertos caracteres al calcular la longitud.

## Ver también

- [Documentación de PHP sobre la función strlen()](https://www.php.net/manual/es/function.strlen.php)
- [Documentación de PHP sobre la función mb_strlen()](https://www.php.net/manual/es/function.mb-strlen.php)
- [Documentación de PHP sobre la función strip_tags()](https://www.php.net/manual/es/function.strip-tags.php)
- [Documentación de PHP sobre la función trim()](https://www.php.net/manual/es/function.trim.php)
- [Documentación de PHP sobre la función str_replace()](https://www.php.net/manual/es/function.str-replace.php)