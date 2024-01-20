---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär helt enkelt att man hämtar sidans data från servern så att den kan visas i en webbläsare. Programmerare gör detta för att kunna analysera, manipulera eller återanvända webbsidans data i deras egna program eller applikationer.

## Så här:
Med PHP kan du använda inbyggda funktioner som `file_get_contents` för att ladda ner en webbsida. Se exempelkoden nedan:

```PHP
<?php
$url = "http://example.com";
$page = file_get_contents($url);

echo $page;
?>
```

När du kör denna kod kommer du att se all HTML-kod från `http://example.com` i din PHP-konsol.

## Fördjupning:
Ladda ner en webbsida var en primär funktion av webbläsare när internet först introducerades. Med tiden har behovet av att automatisera denna uppgift lett till utvecklingen av olika tekniker och metoder för programmeringsspråk, inklusive PHP.

Ett alternativ till `file_get_contents` i PHP kan vara att använda `cURL`-biblioteket, som ger mer kontroll och anpassning, som att hantera cookies eller att ställa in specifika HTTP-headers. 

När du laddar ner en webbsida via PHP, är det viktigt att komma ihåg att PHP-behandlingen sker på servern och inte klientens sida. Detta innebär att dessa data måste laddas varje gång sidan laddas, vilket kan påverka prestanda och belastningstider.

## Se Även:
För ytterligare information om ämnet, se följande länkar:
- PHP Manual: file_get_contents: https://www.php.net/manual/en/function.file-get-contents.php 
- PHP Manual: cURL: https://www.php.net/manual/en/book.curl.php
- Historien om webb nedladdning: https://www.w3.org/History.html