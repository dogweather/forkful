---
title:                "Extrayendo subcadenas"
html_title:           "PHP: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en programación?

Extraer subcadenas, también conocidas como substrings, es una habilidad útil en la programación ya que permite manipular y trabajar con cadenas más específicas de un texto. Esto puede ser especialmente útil en situaciones en las que solo se necesita una parte de una cadena de texto, como en la validación de formularios o en la búsqueda de palabras clave.

## Cómo hacerlo en PHP

Para extraer subcadenas en PHP, se utiliza la función `substr()` que toma tres parámetros: la cadena original, el índice de inicio y la longitud de la subcadena deseada. Por ejemplo, si tenemos una cadena `Hola Mundo` y queremos extraer la subcadena `Mundo`, podemos usar el siguiente código:

```PHP
$cadena = "Hola Mundo";
$subcadena = substr($cadena, 5, 5);
echo $subcadena; // Resultado: Mundo
```

También se pueden utilizar números negativos para indicar el índice de inicio en relación al final de la cadena. Por ejemplo, si queremos extraer la última palabra de una cadena, podemos usar:

```PHP
$cadena = "Este es un ejemplo";
$subcadena = substr($cadena, -7);
echo $subcadena; // Resultado: ejemplo
```

Además, la función `substr()` también se puede utilizar para reemplazar una parte de la cadena con otra subcadena. Por ejemplo, si queremos reemplazar la primera palabra de una cadena con otra, podemos hacerlo de la siguiente manera:

```PHP
$cadena = "Hola Mundo";
$subcadena = substr($cadena, 0, strpos($cadena, " "));
echo $subcadena . " Amigo"; // Resultado: Amigo Mundo
```

## Profundizando en la extracción de subcadenas

Además de la función `substr()`, PHP también cuenta con otras funciones para extraer subcadenas de una manera más específica y avanzada. Algunas de estas son:

- `mb_substr()`: similar a `substr()` pero funciona correctamente con caracteres multibyte.
- `str_split()`: divide una cadena en un array de subcadenas de longitud específica.
- `preg_match()`: permite extraer una subcadena utilizando expresiones regulares.

También es importante tener en cuenta que la extracción de subcadenas en PHP depende del tipo de encoding que se esté utilizando en la cadena original. Por lo tanto, es recomendable revisar la documentación de cada función antes de utilizarlas.

## Ver También

- [Documentación oficial de substr() en PHP](https://www.php.net/manual/es/function.substr.php)
- [Ejemplos de expresiones regulares en PHP](https://www.php.net/manual/es/reference.pcre.pattern.syntax.php)
- [Manual de funciones para manipular cadenas en PHP](https://www.php.net/manual/es/ref.strings.php)