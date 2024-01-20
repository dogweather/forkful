---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La conversión de una cadena a minúsculas es simplemente cambiar todas las letras mayúsculas de una cadena de texto a sus equivalentes en minúsculas. Los programadores lo hacen por muchas razones, pero a menudo es importante para la manipulación de datos y la comparación de cadenas.

## Cómo hacerlo:

Primero, aquí tienes un ejemplo simple en PHP que convierte una cadena a minúsculas usando la función `strtolower()`.

```PHP
<?php
$texto = '¡HOLA, MUNDO!';
$minuscula = strtolower($texto);
echo $minuscula;
?>
```

La salida será: `¡hola, mundo!`

Usando la función `mb_strtolower()`, puedes hacerlo incluso para caracteres multibyte (como por ejemplo los acentos). Aquí tienes un ejemplo:

```PHP
<?php
$texto = "¡HÓLA, MÚNDO!";
$minuscula = mb_strtolower($texto, 'UTF-8');
echo $minuscula;
?>
```

La salida será: `¡hóla, múndo!`

## Buceo profundo

La necesidad de convertir cadenas a minúsculas en PHP ha estado presente desde las primeras versiones del lenguaje y la función `strtolower()` ha sido parte del núcleo de PHP desde su principio. Con el tiempo, con la incorporación de caracteres multibyte y diferentes encodings, se agregó la función `mb_strtolower()`.

Alternativamente, si quieres cambiar una cadena a mayúsculas, puedes utilizar las funciones `strtoupper()` o `mb_strtoupper()`.

En cuanto a los detalles de implementación, `strtolower()` en PHP está construido sobre la función `tolower()` de la biblioteca C standard. Por su parte, `mb_strtolower()` usa una tabla de conversión que va más allá de los caracteres ASCII estándar para incluir también caracteres multibyte.

## Ver también

Para más información acerca de estas funciones y manipulación de cadenas en PHP, puedes visitar:

- Documentación oficial de PHP para [strtolower()](https://www.php.net/manual/en/function.strtolower.php)

- Documentación oficial de PHP para [mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)

- Guía completa de manipulación de cadenas en PHP en [w3schools.com](https://www.w3schools.com/php/php_ref_string.asp)