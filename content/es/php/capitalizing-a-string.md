---
title:                "Mayúscula de una cadena"
html_title:           "PHP: Mayúscula de una cadena"
simple_title:         "Mayúscula de una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena de texto significa poner la primera letra en mayúscula y el resto en minúscula. Los programadores hacen esto para mejorar la legibilidad de sus códigos y seguir las convenciones de escritura establecidas.

## ¿Cómo hacerlo?
Puedes capitalizar una cadena de texto usando la función integrada de PHP `ucwords()`. Simplemente ingresa la cadena como un parámetro y la función devolverá una nueva cadena con la primera letra de cada palabra en mayúscula. Por ejemplo:
```PHP
$string = "hola mundo";
echo ucwords($string); //Salida: Hola Mundo
```

Otra opción es usar la función `strtoupper()` que convierte todas las letras de la cadena en mayúsculas. En este caso, también necesitarías usar la función `strtolower()` para convertir la primera letra en minúscula. Por ejemplo:
```PHP
$string = "hola mundo";
echo strtoupper(substr($string, 0, 1)) . strtolower(substr($string, 1)); //Salida: Hola mundo
```

## Exploración en profundidad
Capitalizar una cadena de texto no es solo una cuestión de estética. En realidad, tiene sus raíces en la tradición de la escritura. En la antigüedad, cuando los textos se escribían a mano, se utilizaba la primera letra en mayúscula para enfatizar el comienzo de una oración. Con el tiempo, esta convención se extendió a los nombres propios y finalmente a todas las palabras en una oración.

Además de las funciones mencionadas anteriormente, también puedes utilizar expresiones regulares para capitalizar una cadena de texto. Sin embargo, ten en cuenta que esta solución puede ser más compleja y puede afectar el rendimiento de tu código.

## Ver también
- [Documentación oficial de PHP para `ucwords()`](https://www.php.net/manual/en/function.ucwords.php)
- [Documentación oficial de PHP para `strtoupper()`](https://www.php.net/manual/en/function.strtoupper.php)
- [Expresiones Regulares en PHP](https://www.php.net/manual/en/book.pcre.php)