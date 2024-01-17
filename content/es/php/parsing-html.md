---
title:                "Analizando HTML"
html_title:           "PHP: Analizando HTML"
simple_title:         "Analizando HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El análisis de HTML es una técnica utilizada por los programadores para extraer información específica de una página web. Esto se hace a través de un proceso de lectura y análisis del código HTML de la página para identificar elementos específicos que contengan la información deseada. Es una forma de automatizar el proceso de extracción de datos de una página web en lugar de hacerlo manualmente.

## Cómo hacerlo:
Para analizar HTML en PHP, utilizamos la función `file_get_contents()` para obtener el código HTML de la página web y luego lo pasamos como argumento a la función `DOMDocument::loadHTML()` para convertirlo en un objeto DOM. A continuación, podemos usar las funciones `getElementsByTagName()` y `getAttribute()` para extraer los elementos y atributos deseados. Aquí hay un ejemplo de cómo obtener el título y la descripción de una página web:

```PHP
$html = file_get_contents("http://www.ejemplo.com");
$dom = new DOMDocument();
$dom->loadHTML($html);

$title = $dom->getElementsByTagName("title")->item(0)->nodeValue;
$description = $dom->getElementsByTagName("meta")->item(0)->getAttribute("content");
```

El variable `$title` contendría entonces el título de la página y `$description` la descripción.

## Profundizando:
El análisis de HTML se ha convertido en una técnica común en la era digital en la que la información está disponible en línea. Esto es especialmente útil para los desarrolladores de aplicaciones web que necesitan acceder a datos de múltiples fuentes y servicios web. Además de PHP, también hay otras herramientas disponibles para analizar HTML como Python y Node.js. Sin embargo, PHP sigue siendo una opción popular debido a su facilidad de uso y su compatibilidad con la mayoría de los servidores web.

## Ver también:
- Documentación oficial de PHP sobre la función `DOMDocument::loadHTML()`: http://php.net/manual/es/domdocument.loadhtml.php
- Tutorial sobre cómo analizar HTML con PHP: https://www.ibm.com/developerworks/library/os-php-parse/index.html
- Alternativas a PHP para analizar HTML: https://www.toptal.com/php/parsing-html-with-php