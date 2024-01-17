---
title:                "Eliminando caracteres que coincidan con un patrón."
html_title:           "PHP: Eliminando caracteres que coincidan con un patrón."
simple_title:         "Eliminando caracteres que coincidan con un patrón."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# ¡Eliminando caracteres que coincidan con un patrón en PHP!

## ¿Qué y por qué?
Eliminar caracteres que coincidan con un patrón es una técnica comúnmente utilizada por los programadores para modificar o limpiar cadenas de texto de manera eficiente. Esto se logra mediante el uso de expresiones regulares, que son patrones de búsqueda que especifican qué caracteres deseamos encontrar y eliminar de una cadena.

Muchas veces, los programadores utilizan esta técnica para validar y asegurar que una cadena de texto cumpla con un determinado formato. También es útil para realizar operaciones de limpieza de datos o para manipular cadenas de texto de forma precisa y rápida.

## ¿Cómo?
Utilizando la función `preg_replace()` podemos especificar un patrón de búsqueda y reemplazo para eliminar los caracteres que coincidan con dicho patrón. Por ejemplo, si deseamos eliminar todas las sustancias blancas de una cadena de texto, podemos hacer lo siguiente:

```PHP
$string = "¡Eliminando caracteres como estos!";
$result = preg_replace("/[aeiou]/i", "", $string);

echo $result; // Salida: ¡lmnnd chrctsrs cm sts!
```

En este caso, hemos utilizado el patrón `/[aeiou]/i` que busca cualquier vocal en minúscula o mayúscula y la elimina de la cadena de texto.

## Profundizando
Eliminar caracteres que coincidan con un patrón ha sido una técnica muy utilizada desde los primeros días de la informática, ya que permite realizar operaciones de búsqueda y reemplazo de manera eficiente. Sin embargo, con el paso del tiempo, han surgido otras alternativas para realizar esta tarea, como los filtros de entrada y salida de PHP, que pueden restringir y modificar el acceso a las cadenas de texto.

En cuanto a la implementación en PHP, la función `preg_replace()` hace uso de la biblioteca PCRE (Perl Compatible Regular Expressions) para realizar la búsqueda y reemplazo. Esta biblioteca es capaz de manejar patrones de búsqueda muy complejos y ofrece una gran flexibilidad en la forma de especificarlos.

## Vea también
Si quieres aprender más acerca de las expresiones regulares y cómo utilizarlas en tus aplicaciones PHP, te recomiendo estos recursos:

- [Documentación oficial de PHP sobre `preg_replace()`](https://www.php.net/manual/en/function.preg-replace.php)
- [Tutorial sobre expresiones regulares en PHP](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Biblioteca PCRE](https://www.pcre.org/)