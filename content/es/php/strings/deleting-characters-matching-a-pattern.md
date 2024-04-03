---
date: 2024-01-20 17:42:54.615591-07:00
description: "C\xF3mo hacerlo: Veamos c\xF3mo eliminar cifras de un string en PHP."
lastmod: '2024-03-13T22:44:59.144779-06:00'
model: gpt-4-1106-preview
summary: "Veamos c\xF3mo eliminar cifras de un string en PHP."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

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
