---
title:                "Eliminando caracteres que coinciden con un patrón"
date:                  2024-01-20T17:42:54.615591-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón es filtrar un string para quitarle ciertas cosas según reglas definidas, como letras o números en particular. Los programadores lo hacen para limpiar datos, como retirar caracteres especiales o validar entradas de usuario.

## Cómo hacerlo:

Veamos cómo eliminar cifras de un string en PHP:

```PHP
<?php
$texto = "Ab3cd9ef";
$patron = '/[0-9]/'; // Esto es una expresión regular que busca dígitos

$resultado = preg_replace($patron, '', $texto);

echo $resultado; // Imprime "Abcdef"
?>
```

Y si queremos quitar puntos y comas:

```PHP
<?php
$otroTexto = "Hola, esto es un texto. Con puntos, y comas!";
$patron = '/[.,]/';  // Busca puntos y comas

$resultadoLimpio = preg_replace($patron, '', $otroTexto);

echo $resultadoLimpio; // "Hola esto es un texto Con puntos y comas"
?>
```

## Inmersión Profunda:

Historia breve: PHP utiliza expresiones regulares (regex) desde sus primeras versiones, basándose en las de Perl, una de las herramientas más potentes para manejo de strings. 

Hay métodos alternativos, como `str_replace()` para casos más sencillos sin patrones complejos. para sustituir cadenas específicas.

Detalles de implementación: `preg_replace()` puede ser más costoso en términos de rendimiento debido a que usa regex, así que es mejor utilizarlo cuando realmente se necesite la flexibilidad que ofrece. Cada patrón regex lleva delimitadores (como `/` en los ejemplos) y puede contener caracteres especiales que definen el patrón a buscar.

## Ver También:

- [PHP manual on preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Regex tutorial](https://www.regular-expressions.info/tutorial.html)
- [PHP manual on str_replace()](https://www.php.net/manual/en/function.str-replace.php)