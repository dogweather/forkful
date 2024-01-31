---
title:                "HTML parsen"
date:                  2024-01-20T15:33:02.790682-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parser helfen dabei, den Inhalt und die Struktur von HTML-Dokumenten zu analysieren. Programmierer nutzen sie, um Daten aus Webseiten zu extrahieren, automatisiert zu bearbeiten oder um HTML-Inhalte für andere Formate aufzubereiten.

## So geht's:
Die Verwendung des `DOMDocument`-Objekts in PHP ist ein einfacher Weg, um HTML zu parsen. Hier ein kurzes Beispiel:

```php
<?php
$htmlString = '<!DOCTYPE html><html><body><h1>Hello World!</h1></body></html>';

$dom = new DOMDocument();
@$dom->loadHTML($htmlString); // Das "@" unterdrückt Warnungen beim HTML-Laden

$headline = $dom->getElementsByTagName('h1')->item(0);

echo $headline->nodeValue; // Gibt "Hello World!" aus
?>
```

Sample output:
```
Hello World!
```

## Deep Dive:
HTML-Parsing in PHP hat eine ziemlich lange Geschichte. Die erste große Bibliothek war Simple HTML DOM, aber sie war relativ langsam und benötigte viel Speicher. PHP's `DOMDocument` verbesserte diese Situation, doch es ist nicht ohne Tücken – insbesondere das richtige Handling von Zeichensätzen kann knifflig sein.

Eine elegante Alternative ist die PHP-Erweiterung `phpQuery` oder `QueryPath`, die ähnlich wie jQuery funktioniert. Während `DOMDocument` auf der libxml2-Bibliothek basiert und eine sehr genaue Analyse ermöglicht, bieten diese Alternativen eine einfachere Syntax für das Navigieren und Manipulieren von HTML-Dokumenten.

Wichtig zu wissen: Das Parsen von schlecht strukturiertem HTML kann zu inkonsistenten Ergebnissen führen, und der Kaschierung-Operator "@" sollte sparsam verwendet werden, da er auch nützliche Fehlermeldungen unterdrückt.

## See Also:
- PHP Manual on DOMDocument: https://www.php.net/manual/en/class.domdocument.php
- Simple HTML DOM Parser: http://simplehtmldom.sourceforge.net/
- phpQuery: https://code.google.com/archive/p/phpquery/
- QueryPath: https://querypath.org/
