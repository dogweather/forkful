---
title:                "Arbeta med yaml"
html_title:           "PHP: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?

YAML är ett utvidgbart dataformat som används för att representera data i en läsbar textform. Programmerare använder det ofta för att strukturera, lagra och överföra data mellan olika system och programmeringsspråk på ett enkelt och standardiserat sätt.

## Så här:

Ett grundläggande exempel på hur man arbetar med YAML i PHP:

```PHP
<?php
// Skapa ett YAML-dokument
$myYaml = "namn: John Smith\nålder: 35\nfavoritfärg: blå";

// Konvertera till ett PHP-objekt
$myObj = yaml_parse($myYaml);

// Hämta och skriv ut data från objektet
$namn = $myObj['namn'];
echo "Mitt namn är $namn. ";

$ålder = $myObj['ålder'];
echo "Jag är $ålder år gammal. ";

$färg = $myObj['favoritfärg'];
echo "Min favoritfärg är $färg.";
?>
```

Detta kommer att ge följande utmatning:
```
Mitt namn är John Smith. Jag är 35 år gammal. Min favoritfärg är blå.
```

## Djupdykning:

YAML utvecklades först 2001 av Clark Evans och Ingy döt Net, men populariteten har vuxit i takt med ökningen av webbapplikationer och API-erfarenheter. Andra liknande format som används för att representera data är XML och JSON, men YAML har som mål att vara enklare och lättare att läsa och skriva än dessa. I PHP finns det också många alternativ för att arbeta med YAML som till exempel symfony/yaml och pecl /yaml, men den inbyggda funktionen yaml_parse är det enklaste sättet att komma igång.

## Se även:

Mer information om YAML och hur man arbetar med det i PHP finns tillgängligt på följande länkar:
- [Officiell YAML-webbplats] (https://yaml.org)
- [PHP dokumentation för yaml_parse] (https://www.php.net/manual/en/function.yaml-parse.php)
- [Symfony Yaml-komponent] (https://symfony.com/doc/current/components/yaml.html)
- [PECL YAML] (https://pecl.php.net/package/yaml)