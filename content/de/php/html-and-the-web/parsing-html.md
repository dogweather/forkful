---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:34.273976-07:00
description: "HTML in PHP zu parsen, bedeutet, spezifische Informationen aus HTML-Dokumenten\
  \ zu extrahieren. Programmierer f\xFChren diese Aufgabe aus, um die\u2026"
lastmod: '2024-02-25T18:49:51.026569-07:00'
model: gpt-4-0125-preview
summary: "HTML in PHP zu parsen, bedeutet, spezifische Informationen aus HTML-Dokumenten\
  \ zu extrahieren. Programmierer f\xFChren diese Aufgabe aus, um die\u2026"
title: HTML parsen
---

{{< edit_this_page >}}

## Was & Warum?
HTML in PHP zu parsen, bedeutet, spezifische Informationen aus HTML-Dokumenten zu extrahieren. Programmierer führen diese Aufgabe aus, um die Datenerfassung zu automatisieren, Web-Scraping durchzuführen oder Inhalte verschiedener Webseiten in ihren Anwendungen zu integrieren, um die Funktionalität ohne manuelles Eingreifen zu erweitern.

## Wie geht das:
Zum Parsen von HTML können PHP-Programmierer eingebaute Funktionen nutzen oder sich auf robuste Bibliotheken wie Simple HTML DOM Parser stützen. Hier werden wir Beispiele anhand von PHPs `DOMDocument` und dem Simple HTML DOM Parser erläutern.

### Unter Verwendung von `DOMDocument`:
Die `DOMDocument`-Klasse von PHP ist Teil seiner DOM-Erweiterung und ermöglicht das Parsen und Manipulieren von HTML- und XML-Dokumenten. Hier ist ein schnelles Beispiel, wie `DOMDocument` verwendet wird, um alle Bilder in einem HTML-Dokument zu finden:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Beispielseite</title>
</head>
<body>
    <img src="image1.jpg" alt="Bild 1">
    <img src="image2.jpg" alt="Bild 2">
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

Beispielausgabe:
```
image1.jpg
image2.jpg
```

### Unter Verwendung von Simple HTML DOM Parser:
Für komplexere Aufgaben oder eine einfachere Syntax bevorzugen Sie möglicherweise eine Drittanbieterbibliothek. Simple HTML DOM Parser ist eine beliebte Wahl und bietet eine jQuery-ähnliche Schnittstelle zum Navigieren und Manipulieren von HTML-Strukturen. So verwenden Sie es:

Zuerst installieren Sie die Bibliothek mit Composer:
```
composer require simple-html-dom/simple-html-dom
```

Dann manipulieren Sie HTML, um beispielsweise alle Links zu finden:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.beispiel.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

Dieser Codeausschnitt wird den HTML-Inhalt von 'http://www.beispiel.com' abrufen, parsen und alle Hyperlinks ausgeben. Denken Sie daran, `'http://www.beispiel.com'` durch die tatsächliche URL zu ersetzen, die Sie parsen möchten.

Durch die Nutzung dieser Methoden können PHP-Entwickler HTML-Inhalte effektiv parsen, die Datenerfassung an ihre Bedürfnisse anpassen oder externe Webinhalte nahtlos in ihre Projekte integrieren.
