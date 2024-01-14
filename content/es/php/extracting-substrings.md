---
title:    "PHP: Extrayendo subcadenas"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en PHP

Extraer subcadenas en PHP es una práctica común en la programación que permite manipular y obtener partes específicas de una cadena de texto. Esto puede ser útil en una variedad de situaciones, como formatear datos o validar entradas de usuarios. En esta publicación, exploraremos por qué es importante aprender a extraer subcadenas y cómo hacerlo de manera efectiva en PHP.

## Cómo hacerlo

Para extraer una subcadena de una cadena en PHP, podemos usar la función `substr ()`. Esta función toma tres parámetros: la cadena original, la posición de inicio y la longitud de la subcadena que deseamos extraer. Aquí hay un ejemplo de cómo usarlo:

````PHP
$cadena = "Hola mundo";
$subcadena = substr($cadena, 0, 4); // Extrae los primeros 4 caracteres
echo $subcadena; // Salida: "Hola"
````

También podemos utilizar índices negativos para contar desde el final de la cadena. Por ejemplo, si queremos extraer los últimos 5 caracteres de una cadena, podemos hacerlo con `substr ($cadena, -5)`.

Otra opción es utilizar la función `mb_substr ()` para manipular cadenas multibyte en PHP. Esta función se asegura de que los caracteres se cuenten correctamente, incluso en diferentes caracteres multibyte como los acentos.

````PHP
$cadena = "¡Bienvenido a mi blog!";
$subcadena = mb_substr($cadena, 13); // Extrae desde el índice 13 hasta el final
echo $subcadena; // Salida: "mi blog!"
````

## Profundizando

Además de la función `substr ()` y `mb_substr ()`, también podemos usar expresiones regulares en PHP para extraer subcadenas. Esto brinda mayor flexibilidad y control sobre qué partes de la cadena deseamos obtener. Por ejemplo, podríamos querer extraer solo números de una cadena o eliminar todos los caracteres que no sean letras.

También es importante tener en cuenta que en PHP, los índices de las cadenas comienzan en 0 y no en 1, lo que puede ser confuso al principio. Por lo tanto, al usar `substr ()`, debemos ser cuidadosos y asegurarnos de usar los índices correctos para obtener la subcadena deseada.

## Ver también

Aquí hay algunos recursos útiles para seguir aprendiendo sobre cómo extraer subcadenas en PHP:

- [Documentación oficial de PHP para la función substr ()](https://www.php.net/manual/es/function.substr.php)
- [Tutorial de w3schools sobre cómo extraer subcadenas en PHP](https://www.w3schools.com/php/func_string_substr.asp)
- [Tutorial en español de Programando en PHP sobre expresiones regulares en PHP](https://programandoenphp.blogspot.com/2018/01/funciones-string-parte-1.html)