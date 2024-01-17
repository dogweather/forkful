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

## ¿Qué y por qué?
Extraer subcadenas es una técnica común en la programación que consiste en obtener una parte específica de una cadena de caracteres. Los programadores lo hacen para manipular datos, validar entradas o realizar operaciones específicas con ciertos caracteres.

## Cómo:
### Ejemplo 1:

```PHP 
$cadena = "Hola Mundo";
$subcadena = substr($cadena, 0, 4);
echo $subcadena;
```

### Salida:
"Hola"

En este ejemplo, utilizamos la función ```substr()``` en PHP para obtener los primeros cuatro caracteres de la cadena "Hola Mundo". Al imprimir la variable ```$subcadena```, obtenemos la subcadena "Hola".

### Ejemplo 2:

```PHP
$cadena = "Lorem ipsum dolor sit amet";
$subcadena = substr($cadena, 6, 5);
echo $subcadena;
```

### Salida:
"ipsum"

En este segundo ejemplo, usamos la función ```substr()``` para obtener una subcadena específica de la cadena "Lorem ipsum dolor sit amet". Con los parámetros proporcionados, obtendremos la subcadena "ipsum" que se encuentra a partir del sexto caracter y con una longitud de 5 caracteres.

## Profundizando:
Extraer subcadenas ha sido una técnica utilizada desde los primeros lenguajes de programación. Aunque en PHP existen otras funciones como ```mb_substr()``` para manejar cadenas multibyte, la función ```substr()``` sigue siendo una de las más utilizadas debido a su simplicidad de uso.

Otra alternativa a la función ```substr()``` es utilizar expresiones regulares, sin embargo, esto puede ser más complejo y menos eficiente en algunos casos.

En cuanto a la implementación de la función ```substr()```, es importante tener en cuenta que los índices de los caracteres en una cadena comienzan desde 0, por lo tanto, el primer caracter de una cadena tendría el índice 0.

## Ver también:
- [Documentación oficial de substr() en PHP] (https://www.php.net/manual/es/function.substr.php)
- [Ejemplos prácticos de uso de substr() en PHP] (https://www.w3schools.com/php/func_string_substr.asp)
- [Comparación entre las funciones substr() y mb_substr() en PHP] (https://stackoverflow.com/questions/19440498/sub-str-or-mb-sub-str-which-one-is-better-to-use-in-php)