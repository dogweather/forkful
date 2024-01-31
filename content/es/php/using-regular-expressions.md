---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Las expresiones regulares son patrones que permiten encontrar y manipular textos de forma avanzada. Los programadores las usan para validaciones, búsqueda, y sustituciones complejas en cadenas de caracteres.

## Cómo Hacerlo:

```PHP
<?php
$texto = "El correo de Ana es ana@example.com y el de Juan es juan@example.net";

//Buscar correos electrónicos usando una expresión regular
preg_match_all('/[a-z]+@[a-z]+\.[a-z]{2,3}/', $texto, $correos);
print_r($correos[0]);

//Sustituir correos encontrados por '***@***.***'
$textoModificado = preg_replace('/[a-z]+@[a-z]+\.[a-z]{2,3}/', '***@***.***', $texto);
echo $textoModificado;
?>
```

Salida:

```
Array
(
    [0] => ana@example.com
    [1] => juan@example.net
)
El correo de Ana es ***@***.*** y el de Juan es ***@***.***
```

## Profundizando:

Las expresiones regulares se originaron en la teoría de autómatas y la lingüística computacional, siendo parte de la programación en los años 60. Como alternativas, podrías usar funciones de cadenas, aunque son menos potentes. Para su implementación, PHP utiliza la biblioteca PCRE (Perl Compatible Regular Expressions), ofreciendo funciones como `preg_match`, `preg_replace`, entre otras.

## Ver También:

- Documentación oficial de PHP sobre expresiones regulares: [php.net/manual/es/book.pcre.php](https://www.php.net/manual/es/book.pcre.php)
- Tutorial interactivo para aprender expresiones regulares: [regexone.com](https://regexone.com)
- Herramienta para probar expresiones regulares en línea: [regex101.com](https://regex101.com)
