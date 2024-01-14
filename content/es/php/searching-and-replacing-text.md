---
title:                "PHP: Buscar y reemplazar texto"
simple_title:         "Buscar y reemplazar texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un desarrollador de PHP, es posible que te encuentres en situaciones donde necesites reemplazar ciertas palabras o frases en un texto. Esto puede ser para corregir errores o para actualizar contenido en una página web. En lugar de hacerlo manualmente, existe una función en PHP que puede facilitarte esta tarea: la función `str_replace()`.

## Cómo hacerlo
La sintaxis básica de `str_replace()` es la siguiente:

```PHP
str_replace( string $reemplazar, string $nuevo, string|array $cadena [, int $conteo ] )
```

En primer lugar, debes definir la palabra o frase que deseas reemplazar en la variable `$reemplazar`. Luego, especifica el nuevo texto que deseas utilizar en la variable `$nuevo`. Estas dos variables pueden ser cadenas de texto, o incluso arreglos de cadenas si necesitas reemplazar varias palabras o frases a la vez.

Por ejemplo, si tienes un texto que dice "Hola mundo!", pero necesitas cambiarlo a "Hola usuarios!", puedes usar la función `str_replace()` de la siguiente manera:

```PHP
$texto = "Hola mundo!";
$nuevo_texto = str_replace("mundo", "usuarios", $texto);
echo $nuevo_texto;
```

Esto mostrará "Hola usuarios!" en la pantalla.

El parámetro opcional `$conteo` te permite especificar cuántas veces deseas hacer el reemplazo en el texto. Por defecto, la función reemplazará todas las coincidencias en la cadena de texto.

## Profundizando
Además de la función `str_replace()`, PHP ofrece otras funciones similares para buscar y reemplazar texto, como `str_ireplace()` que es insensible a mayúsculas y minúsculas, o `preg_replace()` que utiliza expresiones regulares para realizar el reemplazo.

También puedes utilizar `str_replace()` en combinación con otras funciones, como `explode()` para dividir una cadena de texto en un arreglo y luego realizar el reemplazo en uno o varios elementos individualmente.

Recuerda revisar la documentación oficial de PHP para obtener más información y ejemplos de uso.

## Ver también
- Documentación oficial de `str_replace()`: https://www.php.net/manual/es/function.str-replace.php
- Documentación oficial de `explode()`: https://www.php.net/manual/es/function.explode.php
- Documentación oficial de `preg_replace()`: https://www.php.net/manual/es/function.preg-replace.php