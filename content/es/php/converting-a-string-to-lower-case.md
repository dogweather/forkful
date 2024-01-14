---
title:                "PHP: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué
Convertir una cadena de texto a minúsculas es una tarea común en la programación que puede ser útil en diferentes situaciones. Por ejemplo, cuando se trabaja con bases de datos, es importante que los datos estén estandarizados en cuanto a mayúsculas y minúsculas para evitar problemas de búsqueda y comparación.

## Cómo hacerlo
Usar la función `strtolower()` es la forma más común de convertir una cadena de texto a minúsculas en PHP. Esta función toma como argumento la cadena de texto que queremos convertir y devuelve una nueva cadena con todas las letras en minúsculas. Veamos un ejemplo:

```PHP
$texto = "HOLA a todos";
echo strtolower($texto);
```

Este código imprimirá "hola a todos" en pantalla. Como puedes ver, todas las letras se han convertido a minúsculas, incluso la "A" al comienzo de la cadena.

Si queremos convertir una cadena de texto a minúsculas pero mantener las mayúsculas en ciertas letras, podemos usar la función `strtolower()` en combinación con la función `ucfirst()`, que convierte la primera letra de la cadena a mayúscula. Por ejemplo:

```PHP
$texto = "Esta Es Una CADEna de TEXTo";
echo ucfirst(strtolower($texto));
```

Esto imprimirá "Esta es una cadena de texto" en pantalla. La primera letra se ha convertido a mayúscula, pero el resto de las letras permanecen en minúscula.

## Profundizando
¿Qué sucede si queremos convertir una cadena de texto que no está en inglés? En este caso, la función `strtolower()` no es suficiente, ya que sólo funciona con caracteres ASCII. Para manejar texto en otros idiomas, debemos utilizar la función `mb_strtolower()` que soporta caracteres Unicode. Esta función también toma un segundo argumento opcional para especificar la codificación de caracteres.

Otra opción es la función `strtolower()` de la extensión intl de PHP que también soporta caracteres Unicode y utiliza la codificación especificada en la configuración de PHP.

## Ver también
- [Función strtolower() en documentación de PHP](https://www.php.net/manual/es/function.strtolower.php)
- [Función mb_strtolower() en documentación de PHP](https://www.php.net/manual/es/function.mb-strtolower.php)
- [Función strtolower() de la extensión intl en documentación de PHP](https://www.php.net/manual/es/transliterator.transliterate.php)