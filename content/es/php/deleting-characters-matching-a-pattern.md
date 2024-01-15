---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "PHP: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Algunas veces en la programación, nos encontramos con la necesidad de eliminar caracteres que coinciden con un cierto patrón. Puede ser por razones de seguridad, limpieza de datos o simplemente por cuestiones estéticas. En cualquier caso, conocer cómo eliminar caracteres en PHP puede ser una habilidad útil en nuestro arsenal de programación.

## Cómo hacerlo

Eliminar caracteres en PHP puede lograrse de varias maneras, dependiendo de la situación y del patrón a eliminar. A continuación, veremos algunos ejemplos prácticos utilizando la función `preg_replace()`.

En primer lugar, debemos pasar el patrón que deseamos eliminar como el primer parámetro en formato de expresión regular. Por ejemplo, si queremos eliminar todas las vocales de una cadena de texto, podemos utilizar el patrón `/[aeiou]/`.

```PHP
$string = "Hola mundo";
$new_string = preg_replace("/[aeiou]/", "", $string);
echo $new_string; // Hl mnd
```

La función `preg_replace()` también acepta un segundo parámetro, que puede ser una cadena de reemplazo o un arreglo de cadenas. Si queremos reemplazar cada vocal con un espacio, podemos utilizar el siguiente código:

```PHP
$string = "Hola mundo";
$new_string = preg_replace("/[aeiou]/", " ", $string);
echo $new_string; // H l  m nd
```

Si queremos eliminar un patrón específico, podemos utilizar referencias en el patrón para indicar qué caracteres deben ser eliminados. Por ejemplo, si queremos eliminar los caracteres "http://" de una URL, podemos utilizar el siguiente código:

```PHP
$url = "http://www.ejemplo.com";
$new_url = preg_replace("/(http:\/\/)/", "", $url);
echo $new_url; // www.ejemplo.com
```

También es posible utilizar la función `preg_replace()` para eliminar caracteres específicos de una cadena de texto. Por ejemplo, si queremos eliminar los espacios en blanco de una cadena, podemos utilizar el siguiente código:

```PHP
$string = "Esto es una cadena con espacios";
$new_string = preg_replace("/\s+/", "", $string);
echo $new_string; // Estoescadenaconespacios
```

Este ejemplo utiliza el patrón `\s+` para coincidir con uno o más espacios en blanco y luego los reemplaza con una cadena vacía.

## Profundizando

La función `preg_replace()` es muy versátil y nos permite utilizar patrones complejos para eliminar caracteres de manera eficiente. Una de las ventajas de utilizar expresiones regulares es su capacidad para trabajar con patrones dinámicos, utilizando la sintaxis de variables.

Por ejemplo, si queremos eliminar todos los dígitos de una cadena de texto, podemos utilizar el siguiente código:

```PHP
$string = "1A2B3C4D5E";
$new_string = preg_replace("/\d+/", "", $string);
echo $new_string; // ABCDE
```

El patrón `\d+` coincidirá con uno o más dígitos y los eliminará de la cadena. Pero, ¿qué pasa si queremos eliminar únicamente los dígitos pares? Podemos utilizar una referencia en el patrón para indicar que solo queremos eliminar los dígitos que sean divisibles por 2:

```PHP
$string = "1A2B3C4D5E6F7G";
$new_string = preg_replace("/(\d+)[02468]/", "", $string);
echo $new_string; // ABCDEF
```

En este ejemplo, utilizamos la referencia `[\d]` para indicar que solo queremos que coincida con dígitos que sean divisibles por 2.

## Ver también

- [La documentación oficial de PHP sobre la función `preg_replace()`](https://www.php.net/manual/es/function.preg-replace.php)
- [Un tutorial en español sobre expresiones regulares en PHP](https://www.anerbarrena.com/uso-de-expresiones-regulares-en-php-1235/) 
- [Una herramienta online para probar y aprender expresiones regulares](https://regex101.com/)