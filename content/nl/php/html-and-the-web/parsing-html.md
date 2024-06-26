---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:37.901356-07:00
description: 'Hoe: PHP heeft een aantal ingebouwde bibliotheken om HTML te parsen,
  zoals DOMDocument. Hier is een eenvoudig gebruiksvoorbeeld.'
lastmod: '2024-03-13T22:44:50.892669-06:00'
model: gpt-4-0125-preview
summary: PHP heeft een aantal ingebouwde bibliotheken om HTML te parsen, zoals DOMDocument.
title: HTML Parsen
weight: 43
---

## Hoe:
PHP heeft een aantal ingebouwde bibliotheken om HTML te parsen, zoals DOMDocument. Hier is een eenvoudig gebruiksvoorbeeld:

```php
<?php
$htmlString = '<!DOCTYPE html><html><body><h1>Hallo, Wereld!</h1></body></html>';
$dom = new DOMDocument();
@$dom->loadHTML($htmlString); // '@' onderdrukt waarschuwingen veroorzaakt door ongeldige HTML-structuren
$h1Tags = $dom->getElementsByTagName('h1');

foreach ($h1Tags as $tag) {
    echo $tag->nodeValue; // Uitvoer: Hallo, Wereld!
}
?>
```

Dit script geeft uit: `Hallo, Wereld!`

## Diepere Duik
Terug in de vroege dagen van het web, gebruikten we regex en ad-hoc oplossingen om HTML te grijpen, maar het was rommelig. Enter `DOMDocument` en `SimpleXMLElement`, met behoorlijke HTML- en XML-parsing sinds PHP 5. Ze stellen je in staat om HTML te navigeren en manipuleren als een boomstructuur.

Tegenwoordig, terwijl `DOMDocument` je eerste keuze is voor interne parsing, bieden alternatieven zoals `SimpleHTMLDom` en `phpQuery` extra syntactische suiker en kunnen ze vriendelijker zijn voor degenen die uit een JavaScript/jQuery-achtergrond komen.

Intern converteert `DOMDocument` HTML naar een DOM-boom, waardoor het eenvoudig is om specifieke elementen te benaderen, attributen te wijzigen en zelfs het document ter plekke te modificeren. Een cool aspect van `DOMDocument` is de tolerantie voor slechte HTML, het opruimen ervan en je laten werken met echte webpagina's die niet altijd perfect geformatteerd zijn.

## Zie Ook
- [DOMDocument op PHP.net](https://www.php.net/manual/en/class.domdocument.php)
- [SimpleXML voor het afhandelen van basis XML-taken](https://www.php.net/manual/en/book.simplexml.php)
- [simplehtmldom SourceForge Project](https://sourceforge.net/projects/simplehtmldom/)
- [phpQuery GitHub repository](https://github.com/punkave/phpQuery)
