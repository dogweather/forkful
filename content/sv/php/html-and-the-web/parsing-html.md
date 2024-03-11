---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:49.592629-07:00
description: "Att tolka HTML med PHP inneb\xE4r att extrahera specifik information\
  \ fr\xE5n HTML-dokument. Programmerare utf\xF6r denna uppgift f\xF6r att automatisera\u2026"
lastmod: '2024-03-11T00:14:11.366970-06:00'
model: gpt-4-0125-preview
summary: "Att tolka HTML med PHP inneb\xE4r att extrahera specifik information fr\xE5\
  n HTML-dokument. Programmerare utf\xF6r denna uppgift f\xF6r att automatisera\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML med PHP innebär att extrahera specifik information från HTML-dokument. Programmerare utför denna uppgift för att automatisera dataextrahering, webskrapning eller för att integrera innehåll från olika webbsidor inom sina applikationer, vilket förbättrar funktionaliteten utan manuella ingripanden.

## Hur man gör:
För att tolka HTML kan PHP-programmerare använda inbyggda funktioner eller luta sig mot robusta bibliotek som Simple HTML DOM Parser. Här kommer vi att utforska exempel med användning av både PHP:s `DOMDocument` och Simple HTML DOM Parser.

### Använda `DOMDocument`:
PHP:s `DOMDocument`-klass är en del av dess DOM-tillägg, som tillåter tolkning och manipulering av HTML- och XML-dokument. Här är ett snabbt exempel på hur man använder `DOMDocument` för att hitta alla bilder i ett HTML-dokument:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Exempelsida</title>
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

Exempel på utmatning:
```
image1.jpg
image2.jpg
```

### Använda Simple HTML DOM Parser:
För mer komplexa uppgifter eller enklare syntax kan du föredra att använda ett tredjepartsbibliotek. Simple HTML DOM Parser är ett populärt val som erbjuder ett jQuery-liknande gränssnitt för att navigera och manipulera HTML-strukturer. Så här använder du det:

Installera först biblioteket med Composer:
```
composer require simple-html-dom/simple-html-dom
```

Därefter, manipulera HTML för att till exempel hitta alla länkar:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

Denna kodsnutt kommer att hämta HTML-innehållet för 'http://www.example.com', tolka det, och skriva ut alla hyperlänkar. Kom ihåg att ersätta `'http://www.example.com'` med den faktiska URL du önskar tolka.

Genom att använda dessa metoder kan PHP-utvecklare effektivt tolka HTML-innehåll, anpassa dataextrahering till sina behov eller sömlöst integrera externt webbinnehåll i sina projekt.
