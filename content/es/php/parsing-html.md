---
title:                "PHP: Analizando html"
simple_title:         "Analizando html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué
Antes de comenzar a hablar sobre cómo analizar HTML en PHP, es importante entender por qué alguien querría hacerlo. Analizar HTML puede ser extremadamente útil en la construcción de aplicaciones web, ya que permite extraer información específica de una página web, como precios de productos, noticias o datos de contacto. También puede ser útil para identificar errores o problemas en una página web y corregirlos de manera eficiente.

## Cómo hacerlo
PHP tiene una función incorporada llamada "file_get_contents" que permite leer el contenido de una página web. Podemos utilizar esta función para obtener el HTML de una página web y luego analizarlo utilizando la función "preg_match" para encontrar los datos específicos que estamos buscando.

```PHP
<?php
// Obtener el contenido HTML de una página web
$html = file_get_contents('https://www.ejemplo.com/');

// Encontrar y obtener la información específica
preg_match('/<h1>(.*?)<\/h1>/', $html, $titulo); // encontrar el primer título en la página

// Imprimir el título
echo "El título de la página es: " . $titulo[1]; // la información se encuentra en el índice 1 del array $titulo
?>
```

## Profundizando
Además de utilizar la función "preg_match" para encontrar datos específicos, también podemos utilizar "preg_match_all" para encontrar varios elementos que coincidan con un patrón determinado. Otra opción es utilizar una biblioteca externa, como "Simple HTML DOM", que facilita la manipulación de HTML en PHP.

También es importante tener en cuenta que las páginas web pueden cambiar su estructura y el patrón de los datos puede variar, por lo que es necesario tener un código flexible y actualizado para asegurar que siga funcionando de manera correcta.

## Ver también
- [Función file_get_contents en PHP](https://www.php.net/manual/es/function.file-get-contents.php)
- [Función preg_match en PHP](https://www.php.net/manual/es/function.preg-match.php)
- [Simple HTML DOM - biblioteca para manipular HTML en PHP](https://simplehtmldom.sourceforge.io/)