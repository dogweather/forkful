---
title:                "Tolka HTML"
date:                  2024-01-20T15:33:11.862316-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML betyder att man analyserar HTML-koden för att förstå dess struktur och innehåll. Programmerare gör detta för att manipulera, extrahera eller integrera information från webbsidor.

## Hur man gör:
För att parsa HTML i PHP kan du använda DOMDocument klassen. Här är ett enkelt exempel:

```PHP
<?php
// Skapa ett nytt DOMDocument-objekt
$dom = new DOMDocument();

// Ladda HTML från en sträng
$html = "<!DOCTYPE html><html><body><h1>Hej Världen!</h1></body></html>";
@$dom->loadHTML($html);

// Hitta element med taggen h1
$h1 = $dom->getElementsByTagName('h1')->item(0);

// Skriv ut textinnehållet i h1-elementet
echo $h1->textContent;
```

Detta kommer att ge följande output:
```
Hej Världen!
```

## Djupdykning
Historiskt sett har HTML-parsning varit knepigt eftersom HTML ofta är dåligt formaterad eller inte följer standarder. PHP's DOMDocument klass hanterar detta genom att korrekt formatera HTML innan parsningen påbörjas. Alternativ till PHP:s inbyggda funktioner inkluderar bibliotek som SimpleHTMLDom eller avancerade verktyg som XPath för att göra komplexa förfrågningar i dokumentet. Implementationsdetaljerna innefattar att DOMDocument kan hantera både encoding-frågor och olika dokumenttyper (som XHTML).

## Se också
- PHP:s officiella dokumentation på DOMDocument: https://www.php.net/manual/en/class.domdocument.php
- SimpleHTMLDom biblioteket: http://simplehtmldom.sourceforge.net/
- En introduktion till XPath i PHP: https://www.php.net/manual/en/domxpath.query.php