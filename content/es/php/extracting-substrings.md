---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Extraer subcadenas (substrings) es la operación de seleccionar y obtener fragmentos específicos de un string. Los programadores lo hacen para manipular, analizar o modificar datos de texto.

## ¿Cómo hacerlo?

Aquí examinamos la función `substr()` en PHP que nos permite extraer subcadenas:

```PHP
<?php
$cadena = "¡Hola, mundo!";
$subcadena = substr($cadena, 0, 5);
echo $subcadena;
?>

```

Output esperado:

```PHP
¡Hola
```

Esta función toma tres argumentos: la cadena fuente, el índice de inicio (base 0), y la longitud de la subcadena.

## Inmersión Profunda

Historia: PHP se ha beneficiado de funciones como `substr()` desde su concepción temprana, lo que facilita las tareas de manipulación de cadenas.

Alternativas: Existen otras funciones como `mb_substr()` para extraer subcadenas, fundamental en el manejo de cadenas multibyte (como caracteres Unicode).

Detalles de implementación: Cuando usamos `substr()`, si el segundo parámetro es un número positivo, PHP leerá desde el inicio de la cadena. Si es negativo, PHP leerá desde el final.

## Ver También

Para profundizar en este tema:

1. [Documentación oficial de PHP sobre `substr()`](http://php.net/manual/en/function.substr.php)
2. [Documentación oficial de PHP sobre `mb_substr()`](http://php.net/manual/en/function.mb-substr.php)
3. [Guía de manipulación de cadenas en PHP por W3Schools](https://www.w3schools.com/php/php_ref_string.asp)

Vale la pena explorar estas opciones y entender cuál es mejor para tus necesidades específicas de programación.