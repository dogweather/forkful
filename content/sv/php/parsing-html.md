---
title:                "PHP: Parsa html"
simple_title:         "Parsa html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-html.md"
---

{{< edit_this_page >}}

## Varför?

Att parsra HTML kan vara ett användbart verktyg för att skrapa och analysera data från olika webbsidor. Genom att parsa HTML-kod kan man extrahera specifika delar av informationen och använda den för olika ändamål. Det kan till exempel vara för att skapa en egen sökmotor eller för att automatisera processen att hämta information från flera olika källor.

## Hur gör man?

För att parsra HTML med PHP finns det flera olika verktyg och metoder att använda sig av. Ett vanligt sätt är att använda inbyggda PHP-funktioner såsom "file_get_contents()" och "preg_match()" för att hämta och filtrera HTML-koden.

```PHP
// Hämta HTML-kod från en webbsida
$html = file_get_contents('https://www.example.com');

// Extrahera länkar från HTML-koden med hjälp av regex
preg_match_all('/<a\s+href="(.*?)"/i', $html, $matches);
```

En annan metod som också är vanlig är att använda sig av tredjepartsbibliotek, till exempel "Simple HTML DOM" eller "Goutte". Dessa bibliotek ger mer avancerade funktioner för att parsra och navigera genom HTML-koden.

```PHP
// Använda Simple HTML DOM för att hitta och hämta specifika element från HTML-koden
$html = file_get_html('https://www.example.com');
$links = $html->find('a');

// Använda Goutte för att klicka på länkar och hämta data från en dynamisk webbsida
$goutte = new Goutte\Client();
$crawler = $goutte->request('GET', 'https://www.example.com');
$crawler->filter('a')->first()->click(); // klickar på den första länken på sidan
```

Det finns också andra metoder, beroende på vilken typ av HTML-kod man vill parsra och vilka specifika behov man har.

## Djupdykning

Att parsra HTML kan vara en utmaning då HTML-koden kan vara väldigt varierande och ofta inte är konsekvent. Det krävs ofta en kombination av olika tekniker och en god förståelse av HTML-strukturen för att kunna parsra data på ett tillförlitligt sätt.

En viktig aspekt att tänka på är också att vara säker på vilken data man faktiskt vill hämta och att ha en robust kod som hanterar eventuella fel och oförutsedda situationer.

## Se också

- [PHP dokumentation för "file_get_contents()"](https://www.php.net/manual/en/function.file-get-contents.php)
- [Regex cheat sheet för HTML](https://htmlcheatsheet.com/regex/)
- [Simple HTML DOM dokumentation](https://simplehtmldom.sourceforge.io/)
- [Goutte dokumentation](https://goutte.readthedocs.io/en/latest/)