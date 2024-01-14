---
title:                "PHP: Encontrando la longitud de una cadena"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si estás en el mundo de la programación, seguramente ya te has encontrado con la necesidad de encontrar la longitud de una cadena de texto. Ya sea para validar un campo de formulario o para manipular strings en tu código, conocer la longitud de una cadena puede ser fundamental. En este artículo, te explicaré cómo encontrar la longitud de una cadena en PHP.

## Cómo hacerlo

Existen dos formas de encontrar la longitud de una cadena en PHP: utilizando la función `strlen()` o accediendo a la propiedad `length` de un string. Veamos un ejemplo de cada uno:

```PHP
$cadena = "¡Hola Mundo!";

echo strlen($cadena); // Salida: 11

echo $cadena->length; // Salida: 11
```

Como puedes ver, ambas opciones nos dan el mismo resultado. Sin embargo, es importante tener en cuenta que la función `strlen()` únicamente funciona con cadenas de texto, mientras que la propiedad `length` puede ser aplicada a diferentes tipos de datos. Por ejemplo:

```PHP
$numero = 1234;

echo strlen($numero); // Salida: Nothing

echo $numero->length; // Salida: Error
```

Si necesitas usar la función `strlen()` en un número, deberás convertirlo a una cadena de texto primero.

## Profundizando

La función `strlen()` cuenta el número de caracteres en una cadena, incluyendo espacios en blanco y caracteres especiales como acentos. Pero si quieres contar el número de palabras en una cadena, puedes usar la función `str_word_count()`. También puedes usar la función `mb_strlen()` si necesitas contar caracteres en una cadena multibyte (como por ejemplo, en idiomas como el chino o el ruso).

Otra forma de encontrar la longitud de una cadena de texto es utilizando un ciclo `for` e ir contando cada carácter hasta llegar al final. Sin embargo, esto es más ineficiente que utilizar las funciones ya mencionadas.

## Ver también

- [Documentación oficial de PHP sobre la función `strlen()`](https://www.php.net/manual/es/function.strlen.php)
- [Documentación oficial de PHP sobre la propiedad `length`](https://www.php.net/manual/es/language.types.string.php)
- [Documentación oficial de PHP sobre la función `str_word_count()`](https://www.php.net/manual/es/function.str-word-count.php)
- [Documentación oficial de PHP sobre la función `mb_strlen()`](https://www.php.net/manual/es/function.mb-strlen.php)