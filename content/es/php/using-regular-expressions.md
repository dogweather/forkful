---
title:                "Utilizando expresiones regulares"
html_title:           "PHP: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Las expresiones regulares son secuencias de caracteres que nos permiten realizar búsquedas y manipulaciones avanzadas en cadenas de texto. Los programadores las utilizan para ahorrar tiempo en la validación y manipulación de datos, así como para realizar tareas específicas como buscar patrones en un texto o reemplazar caracteres específicos.

## Cómo Hacerlo:

Para utilizar expresiones regulares en PHP, primero necesitamos utilizar la función `preg_match()` que toma dos argumentos: una expresión regular y una cadena de texto en la que se realizará la búsqueda. Por ejemplo, si queremos buscar la palabra "Hola" en una cadena de texto, podemos utilizar la siguiente expresión regular:

```PHP
<?php
    $cadena = "Hola, ¿cómo estás?";
    if (preg_match("/Hola/", $cadena)) {
        echo "Encontré la palabra 'Hola' en la cadena de texto.";
    } else {
        echo "No se encontró la palabra 'Hola' en la cadena de texto.";
    }
?>
```

El resultado de este código será: "Encontré la palabra 'Hola' en la cadena de texto." Esto se debe a que la expresión regular coincide con la palabra "Hola" en la cadena de texto.

También podemos utilizar expresiones regulares para validar datos, como por ejemplo, verificar si una cadena de texto contiene un número de teléfono válido:

```PHP
<?php
    $telefono = "123-456-7890";
    if (preg_match("/^[0-9]{3}-[0-9]{3}-[0-9]{4}$/", $telefono)) {
        echo "El número de teléfono es válido.";
    } else {
        echo "El número de teléfono no es válido.";
    }
?>
```

El resultado sería: "El número de teléfono es válido."

## Profundizando:

Las expresiones regulares tienen su origen en la teoría de lenguajes formales y la matemática. Fueron desarrolladas en la década de 1950 por Stephen Cole Kleene como una forma de describir lenguajes y patrones en un formato sencillo. Aunque también hay otras formas de realizar búsquedas y manipulación de texto en PHP, como por ejemplo las funciones `strpos()` y `str_replace()`, las expresiones regulares son más poderosas y flexibles.

Si deseas aprender más sobre cómo utilizar expresiones regulares en PHP, puedes consultar la documentación oficial de PHP o buscar tutoriales en línea. También puedes utilizar herramientas en línea para generar expresiones regulares y probarlas en tiempo real.

## Ver También:

- Documentación oficial de PHP sobre expresiones regulares: https://www.php.net/manual/es/reference.pcre.pattern.syntax.php
- Tutorial de expresiones regulares en PHP: https://www.w3schools.com/php/php_regex.asp
- Herramienta para generar y probar expresiones regulares: https://regex101.com/