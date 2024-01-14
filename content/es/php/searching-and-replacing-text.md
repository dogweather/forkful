---
title:                "PHP: Buscando y reemplazando texto."
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

A la hora de escribir código en PHP, a veces es necesario realizar cambios en un texto para mejorar su funcionalidad o corregir errores. Para esto, es importante saber cómo buscar y reemplazar texto de forma eficiente.

## Cómo hacerlo

Para realizar una búsqueda y reemplazo de texto en PHP, se puede utilizar la función `str_replace()`. Esta función toma tres parámetros: el texto a buscar, el texto de reemplazo y el texto completo donde se realizará la búsqueda. Por ejemplo:

```PHP
$texto = "Hola mundo!";
echo str_replace("mundo", "amigos", $texto);
```

La salida de este código sería "Hola amigos!".

También se pueden utilizar expresiones regulares para realizar búsquedas más específicas. Por ejemplo, para reemplazar todos los números en un texto por un asterisco, se puede utilizar la siguiente expresión regular:

```PHP
$texto = "Este es un texto con números: 12345";
echo preg_replace("/[0-9]+/", "*", $texto);
```

La salida sería "Este es un texto con números: *****".

## Profundizando

Si se necesita realizar búsquedas y reemplazos más complejos, se puede utilizar la función `preg_replace_callback()`. Esta función permite utilizar una función personalizada para realizar el reemplazo en lugar de un string fijo. Por ejemplo:

```PHP
// Función que toma el primer y último caracter y los intercambia
function intercambiar($match) {
  return $match[2] . $match[1];
}

$texto = "Hola mundo!";
echo preg_replace_callback("/([a-z]+) ([a-z]+)/i", "intercambiar", $texto);
```

La salida sería "mundo Hola!".

Otra herramienta útil es `strtr()`, que permite realizar múltiples reemplazos en un solo paso. Por ejemplo:

```PHP
$texto = "Este es un ejemplo con varios cambios";
$reemplazos = array("ejemplo" => "texto", "varios" => "muchos");
echo strtr($texto, $reemplazos);
```

La salida sería "Este es un texto con muchos cambios".

## Ver también

- [Documentación oficial de str_replace() en PHP.net](https://www.php.net/manual/es/function.str-replace.php)
- [Información sobre expresiones regulares en PHP](https://www.php.net/manual/es/reference.pcre.pattern.syntax.php)
- [Documentación oficial de preg_replace_callback() en PHP.net](https://www.php.net/manual/es/function.preg-replace-callback.php)
- [Más información sobre la función strtr() en PHP.net](https://www.php.net/manual/es/function.strtr.php)