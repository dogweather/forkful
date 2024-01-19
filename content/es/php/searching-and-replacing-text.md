---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Buscar y reemplazar texto es básicamente cambiar una cadena de texto específica con otra en un documento de programa. Los programadores lo hacen para modificar un gran volumen de información rápidamente o corregir un error recurrente en el script.

## Cómo se hace:
Con PHP, podemos usar la función str_replace. Aquí tienes un ejemplo:

```PHP
<?php 
$texto = "Hola, Mundo!";
$texto = str_replace("Mundo", "a todos", $texto); 
echo $texto; 
?>
```
La salida será: 

```PHP
"Hola, a todos!"
```

En este ejemplo, "Mundo" es la cadena de texto que buscamos y queremos reemplazar con "a todos". 

## Profundizando
La función str_replace ha existido desde PHP 4 y forma parte integral de las cadenas de texto y su manipulación en PHP. Sin embargo, hay alternativas como preg_replace que permite más complejidad al utilizar expresiones regulares. En cuanto a la ejecución, str_replace realiza la búsqueda y el reemplazo de un modo secuencial, es decir, comienza por el inicio de la cadena y acaba en el fin.

## Ver también
Para más información y ejemplos:
- Referencia oficial de PHP en str_replace [aquí](https://www.php.net/manual/es/function.str-replace.php).
- Para aprender más sobre expresiones regulares y preg_replace, consulta [este link](https://www.php.net/manual/es/function.preg-replace.php).