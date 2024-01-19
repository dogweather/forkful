---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?

La concatenación de cadenas se refiere a conectar dos o más cadenas juntas para formar una sola cadena. Los programadores lo hacen para manejar y manipular información en sus códigos de manera más efectiva.

## Cómo se hace:

En PHP, usamos el operador de punto (.) para concatenar cadenas. Mira el código de ejemplo:

```PHP
<?php
$stringUno = "¡Hola";
$stringDos = ", Mundo!";

$resultado = $stringUno . $stringDos;
echo $resultado;
//Imprime: ¡Hola, Mundo!
?>
```
## Análisis en Profundidad:

Históricamente, PHP ha usado siempre el operador de punto (.) para la concatenación de cadenas. Esta facilidad ha hecho a PHP uno de los lenguajes más fáciles para manipular cadenas.

En cuanto a las alternativas, es posible usar la función `sprintf` o la sintaxis de comillas dobles. Por ejemplo:

```PHP
$stringUno = "¡Hola";
$stringDos = ", Mundo!";
$resultado = sprintf('%s%s', $stringUno, $stringDos);
echo $resultado;
//Imprime: ¡Hola, Mundo!
```
En realidad, preferimos el operador de concatenación porque suele ser más rápido y legible que otras opciones.

## Ver También:

Para más detalles acerca de concatenación de cadenas en PHP, se puede consultar las fuentes oficiales de PHP:

1. [Operadores de cadena PHP](https://www.php.net/manual/es/language.operators.string.php)

2. [Función sprintf](https://www.php.net/manual/es/function.sprintf.php)

3. [Las cadenas y sus detalles en PHP](https://www.php.net/manual/es/language.types.string.php)