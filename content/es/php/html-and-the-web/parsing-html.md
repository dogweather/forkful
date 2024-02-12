---
title:                "Analizando HTML"
aliases:
- /es/php/parsing-html/
date:                  2024-02-03T19:12:51.801086-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analizando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Analizar HTML en PHP implica extraer información específica de documentos HTML. Los programadores realizan esta tarea para automatizar la extracción de datos, el web scraping o para integrar contenido de varias páginas web en sus aplicaciones, mejorando la funcionalidad sin intervención manual.

## Cómo hacerlo:
Para analizar HTML, los programadores de PHP pueden utilizar funciones integradas o apoyarse en bibliotecas robustas como Simple HTML DOM Parser. Aquí, exploraremos ejemplos usando tanto `DOMDocument` de PHP como el Simple HTML DOM Parser.

### Usando `DOMDocument`:
La clase `DOMDocument` de PHP es parte de su extensión DOM, permitiendo el análisis y manipulación de documentos HTML y XML. Aquí hay un ejemplo rápido de cómo usar `DOMDocument` para encontrar todas las imágenes en un documento HTML:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Página de ejemplo</title>
</head>
<body>
    <img src="image1.jpg" alt="Imagen 1">
    <img src="image2.jpg" alt="Imagen 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

Salida de ejemplo:
```
image1.jpg
image2.jpg
```

### Usando Simple HTML DOM Parser:
Para tareas más complejas o una sintaxis más fácil, podrías preferir usar una biblioteca de terceros. Simple HTML DOM Parser es una opción popular, proporcionando una interfaz similar a jQuery para navegar y manipular estructuras HTML. Así es cómo usarlo:

Primero, instala la biblioteca usando Composer:
```
composer require simple-html-dom/simple-html-dom
```

Luego, manipula HTML para, por ejemplo, encontrar todos los enlaces:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

Este fragmento de código obtendrá el contenido HTML de 'http://www.example.com', lo analizará e imprimirá todos los hipervínculos. Recuerda reemplazar `'http://www.example.com'` con la URL real que deseas analizar.

Utilizando estos métodos, los desarrolladores de PHP pueden analizar efectivamente contenido HTML, personalizar la extracción de datos según sus necesidades o integrar sin problemas contenido web externo en sus proyectos.
