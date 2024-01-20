---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

La interpolación de cadenas es el proceso de sustituir valores de variables dentro de una cadena. Los programadores lo hacen para emitir o registrar información de manera dinámica.

## Cómo hacerlo:

Aquí hay un pequeño fragmento de código que muestra cómo se realiza la interpolación de cadenas en PHP:

```PHP
$días = 7;
echo "Hay $días en una semana";
```
Esta pieza de código imprimirá: "Hay 7 días en una semana".

## Análisis profundo

La interpolación de cadenas en PHP no solo es práctica, sino que también forma parte de las raíces históricas del lenguaje. Creado en 1994, PHP no tenía espacios de nombres, funciones anónimas ni traits. Sin embargo, desde el principio, PHP tenía interpolación de cadenas.

Las alternativas a la interpolación de cadenas incluyen concatenación de cadenas y el uso de funciones como `sprintf()` o `printf()`. Pero la interpolación puede ser más legible y menos propensa a errores.

Aquí un detalle de implementación que puede interesarle: en su código PHP, si desea incluir variables más complejas (como un elemento de matriz) en una cadena, necesitará encerrarlas en llaves `{}`. Por ejemplo:

```PHP
$frutas = ['manzana', 'banana'];
echo "Me gusta la {$frutas[1]}";
```
Este script imprimirá: "Me gusta la banana".

## Ver también

Para aprender más sobre la interpolación de cadenas en PHP, consulta los siguientes recursos:

1. [PHP: Strings - Manual](http://php.net/manual/en/language.types.string.php)
2. [PHP: String interpolation - Stack Overflow](https://stackoverflow.com/questions/6918393/php-string-interpolation-vs-concatenation)
3. [Interpolation or concatenation? - PHP The Right Way](https://phptherightway.com/#interpolation_or_concatenation)