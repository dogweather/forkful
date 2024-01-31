---
title:                "Análisis de HTML"
date:                  2024-01-20T15:33:01.181166-07:00
html_title:           "Arduino: Análisis de HTML"
simple_title:         "Análisis de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Parsear HTML es extraer información de documentos HTML. Los programadores lo hacen para manipular, analizar o mostrar selectivamente contenido de la web.

## Cómo hacerlo:
```PHP
<?php
$dom = new DOMDocument();
libxml_use_internal_errors(true); // Evitar warnings de HTML mal formado
$dom->loadHTML(file_get_contents('https://example.com'));

$xpath = new DOMXPath($dom);
$nodes = $xpath->query('//h1'); // Aquí buscas elementos h1

foreach ($nodes as $node) {
    echo $node->nodeValue . "\n";
}
```
Salida esperada:
```
El título de la página
```

## Análisis Profundo
Parsear HTML con PHP ha evolucionado con el tiempo. Antiguamente, se usaba `regex` o expresiones regulares, lo cual no es recomendable dado que HTML no es un lenguaje regular y su estructura puede ser muy compleja. Ahora, con `DOMDocument` y `DOMXPath`, PHP ofrece herramientas sólidas y potentes que entienden la estructura del HTML y permiten un análisis más preciso. Alternativas como `SimpleXML` también están disponibles pero tienen sus limitaciones. La implementación correcta dependerá de los requerimientos específicos del proyecto, como rendimiento, complejidad del HTML y necesidades de manipulación del DOM.

## Ver También
- [Documentación oficial de DOMDocument](https://www.php.net/manual/es/class.domdocument.php)
- [Documentación oficial de DOMXPath](https://www.php.net/manual/es/class.domxpath.php)
- [Tutorial de web scraping con PHP y XPath](https://www.phpsimple.net/tutorials/web-scraping-with-xpath)
- [Comparación entre SimpleXML y DOMDocument](https://stackoverflow.com/questions/2981733/simplexml-vs-domdocument)
