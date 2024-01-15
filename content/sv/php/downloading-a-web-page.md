---
title:                "Hämta en webbsida"
html_title:           "PHP: Hämta en webbsida"
simple_title:         "Hämta en webbsida"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför
Att ladda ner en webbsida kan vara ett användbart verktyg för webbutvecklare eller datavetare för att analysera innehållet på en specifik sida. Det kan också användas för att skapa backup eller arkiv av en sida.

## Hur du gör det
Det finns flera sätt att ladda ner en webbsida i PHP, men det enklaste sättet är att använda PHPs inbyggda funktion "file_get_contents ()". Till exempel, om vi vill ladda ner den svenska versionen av Wikipedia's huvudsida:

```PHP
<?php
// Hämta innehållet på en sida
$wiki_sida = file_get_contents("https://sv.wikipedia.org/wiki/Huvudsida");

// Skriv ut innehållet på skärmen
echo $wiki_sida;
?>
```

**Sample output:**
Textversionen av Wikipedia's huvudsida.

## Djupdykning
För mer avancerade användare som är intresserade av att ladda ner fler än en sida eller en hel sajt, finns det mer avancerade lösningar såsom att använda "cURL" eller "wget", vilka båda kan hantera nedladdning av flera sidor och hantera eventuella autentiseringsproblem.

Det finns också många tredjepartsbibliotek som kan hjälpa till att ladda ner och hantera webbsidor i PHP, såsom "Guzzle" och "Crawler." Dessa kan vara användbara för mer komplicerade uppgifter som att extrahera specifikt innehåll eller följa länkar på en sida.

## Se även
 - [PHPs officiella dokumentation om file_get_contents ()](https://www.php.net/manual/en/function.file-get-contents.php)
 - [Guzzle](https://docs.guzzlephp.org/en/stable/), en tredjepartsbibliotek för att hantera HTTP-förfrågningar i PHP.
 - [Crawler](https://github.com/danwriggins/crawler), ett PHP-bibliotek för att ladda ner och analysera webbsidor.