---
title:                "Parsera html"
html_title:           "PHP: Parsera html"
simple_title:         "Parsera html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

När man arbetar med webbutveckling och PHP blir det ofta nödvändigt att hämta data från olika webbsidor. För att kunna hämta specifika delar av informationen från dessa sidor, behöver man kunna tolka och utvinna data från HTML-kod. Detta kallas för "parsning" och är en viktig del i att skapa dynamiska webbsidor och applikationer.

## Så här gör man

För att kunna parsar HTML i PHP finns det flera olika bibliotek och verktyg att använda sig av. Ett av de vanligaste är Simple HTML DOM, som gör det enkelt att navigera och utvinna information från HTML-dokument. Här är ett exempel på hur man kan hämta all text från ett <h1>-element från en webbsida:

```PHP
$html = file_get_html('www.example.com');
// Hämtar all HTML-kod från webbsidan

$h1 = $html->find('h1', 0);
// Använder find-metoden för att hitta det första <h1>-elementet

echo $h1->plaintext;
// Skriver ut den hämtade texten
```

Detta är bara ett enkelt exempel på hur man kan använda sig av parsning i PHP, det finns många fler funktioner och metoder för att navigera och utvinna data från HTML-dokument.

## Djupdykning

Vid mer avancerad parsning finns det också möjlighet att använda sig av regex (regular expressions) för att matcha och utvinna specifika delar av HTML-koden. Det kan också vara användbart att läsa på om DOM-dokumentet (Document Object Model) som beskriver den hierarkiska strukturen av HTML-element.

Det kan också vara värt att notera att parsning av HTML kan vara känsligt för ändringar i sidans struktur och layout. Det är därför viktigt att regelbundet testa och anpassa koden om det skulle bli förändringar på sidan man hämtar data från.

## Se även

* [Simple HTML DOM](http://simplehtmldom.sourceforge.net/)
* [PHP DOM](https://www.php.net/manual/en/book.dom.php)
* [Regex Tutorial](https://www.rexegg.com/regex-quickstart.html)