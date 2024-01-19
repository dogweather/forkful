---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-html.md"
---

{{< edit_this_page >}}

# Parseo de HTML en PHP

## ¿Qué y por qué?

Parsear HTML te permite manipular y extraer información de una página HTML. Los programadores lo hacen para scrappear datos de la web, cambiar contenido dinámicamente, entre otros.

## Como se hace:

Aquí te presento un simple código usando la librería integrada en PHP: DOMDocument.

```PHP
<?php
$dom = new DOMDocument();
@$dom->loadHTML('<h1>Hola Mundo</h1>');

$h1 = $dom->getElementsByTagName('h1');

foreach ($h1 as $element) {
    echo $element->nodeValue, PHP_EOL;
}
?>
```

Este script te imprimirá:

```
Hola Mundo
```

## Un vistazo más profundo:

1. **Contexto histórico**: En el pasado, los programadores debían usar expresiones regulares para parsear HTML. Esto fue complicado y propenso a errores. Hasta que llegó DOMDocument en PHP5 simplificó mucho las cosas.
2. **Alternativas**: Hay otras formas de parsear HTML en PHP. Puedes usar SimpleHTML DOM, ganas en simplicidad pero pierdes en rendimiento. Otra opción es usar la XML extension con el método simplexml_load_string() aunque es menos robusto.
3. **Detalles de implementación**: Recuerda que DOMDocument tiene problemas con HTML mal formado. Si tienes ese problema, te recomiendo usar la librería Tidy antes de parsear.

## Ver también:

1. [Documentación oficial de PHP DOMDocument](http://php.net/manual/es/class.domdocument.php)
2. [SimpleHTML DOM](http://simplehtmldom.sourceforge.net/)
3. [Librería Tidy](http://php.net/manual/en/book.tidy.php)