---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Eliminar caracteres que coinciden con un patrón es básicamente la manipulación de una cadena de caracteres mediante la eliminación de ciertos caracteres. Los programadores lo hacen para limpiar datos, como eliminar espacios extra en blanco, caracteres ilegales o específicos para procesar posteriormente los datos de manera efectiva.

## Cómo hacerlo
Vamos a ver una simple demostración de cómo puede hacerlo con la función `preg_replace` en PHP.

```PHP
<?php
$frase = "Hola, soy tu PHP amigo!";
$patron = "/[aeyou]/i";
$salida = preg_replace($patron, "", $frase);

echo $salida;
// Salida: "Hl, s t PHP mig!"
?>
```

La función `preg_replace` utiliza una expresión regular para buscar coincidencias en nuestra cadena y las reemplaza con lo que le pidamos. Aquí, estamos reemplazando todas las vocales (especificadas en nuestro patrón) por nada, esencialmente eliminándolas.

## Un vistazo más detallado
Borrar caracteres que coinciden con un patrón ha sido una característica en la mayor parte de la historia de la programación, pero su implementación ha evolucionado con el tiempo, definitivamente esencial para trabajar con cadenas en PHP.

Una alternativa a `preg_replace` podría ser la función `str_replace`. Sin embargo, `str_replace` no admite patrones de expresiones regulares, lo cual limita su utilidad en ciertos casos.

¿Cómo funciona detras de escena `preg_replace`? Al recibir una cadena, un patrón y una cadena de reemplazo, recorre todo el texto y para cada carácter, comprueba si coincide con el patrón provisto. Si es así, lo reemplaza con la cadena de reemplazo.

## Ver También
Para obtener más información sobre las funciones de manipulación de cadenas y expresiones regulares en PHP, aquí te dejo algunos enlaces útiles:

1. [PHP: preg_replace - Manual](https://www.php.net/manual/es/function.preg-replace.php)
2. [PHP: str_replace - Manual](https://www.php.net/manual/es/function.str-replace.php)
3. [Expresiones Regulares en PHP](https://www.codigonaranja.com/articulos/expresiones-regulares-php)