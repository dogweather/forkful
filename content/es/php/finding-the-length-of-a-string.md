---
title:    "PHP: Encontrando la longitud de una cadena"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Por qué

En el mundo de la programación, hay veces en las que necesitamos saber la longitud de una cadena de texto. Esto puede parecer un concepto simple, pero saber la longitud de una cadena es esencial al manipular y procesar datos. Sigue leyendo para aprender cómo encontrar la longitud de una cadena en PHP.

##Cómo hacerlo

Para encontrar la longitud de una cadena en PHP, utilizamos la función `strlen()`. Esta función toma una cadena como argumento y devuelve su longitud como un número entero. A continuación se muestra un ejemplo de cómo utilizar esta función en un código PHP:

```PHP
$cadena = "¡Hola, mundo!";
echo strlen($cadena);
```

El código anterior imprimirá `13` en la pantalla, ya que la cadena tiene 13 caracteres. También puedes asignar el valor devuelto por `strlen()` a una variable y utilizarla en tu código de la forma que desees.

##Profundidad

Ahora que sabes cómo usar la función `strlen()` para encontrar la longitud de una cadena, es importante entender cómo funciona en realidad. En PHP, cada cadena se almacena como una serie de bytes, que en términos simples, son una secuencia de números que representan los caracteres en la cadena. La función `strlen()` simplemente cuenta el número de bytes en una cadena, y ese número es lo que se devuelve.

Es importante tener en cuenta que la función `strlen()` no cuenta los espacios en blanco al final de una cadena. Por lo tanto, si tienes un espacio al final de una cadena, este no se incluirá en la longitud devuelta. Es bueno tener esto en cuenta al tratar con datos y asegurarte de que estás obteniendo la longitud correcta.

##Ver también

- [Documentación oficial de PHP sobre la función `strlen()`](https://www.php.net/manual/es/function.strlen.php)
- [Artículo sobre otras formas de encontrar la longitud de una cadena en PHP](https://www.w3schools.com/php/func_string_strlen.asp)
- [Más información sobre cómo se almacenan las cadenas en PHP](https://www.tutorialrepublic.com/php-tutorial/php-strings.php)