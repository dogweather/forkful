---
title:                "Interpolando una cadena"
html_title:           "PHP: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Interpolar una cadena en programación se refiere a insertar valores de variables dentro de una cadena de texto. Los programadores lo hacen para crear cadenas dinámicas que cambian según el valor de las variables. Esto es útil para mostrar mensajes personalizados, generar consultas de bases de datos y más.

## Cómo hacerlo:

Para interpolar una cadena en PHP, usamos el operador de concatenación de cadenas (.) para combinar texto y variables. Por ejemplo:

```PHP
$nombre = "María";
echo "¡Hola, " . $nombre . "!"; // Salida: ¡Hola, María!
```

También podemos utilizar llaves y el símbolo de dólar (${}) para interpolar variables dentro de cadenas con comillas dobles. Por ejemplo:

```PHP
$nombre = "Juan";
echo "¡Hola, {$nombre}!"; // Salida: ¡Hola, Juan!
```

## Profundizando:

Interpolar cadenas ha existido desde los primeros días de PHP, pero a veces se discute si su uso es el más eficiente. Alternativas como el uso de comillas simples en lugar de dobles o el uso de la función sprintf() también pueden ser utilizados para crear cadenas dinámicas. Sin embargo, interpolar cadenas sigue siendo una práctica común y comúnmente aceptada en la programación en PHP.

## Ver también:

- [Documentación oficial de PHP sobre cadenas interpoladas](https://www.php.net/manual/es/language.types.string.php#language.types.string.syntax.double)
- [Artículo de PHP sobre la función sprintf()](https://www.php.net/manual/es/function.sprintf.php)