---
title:                "Att tolka html"
html_title:           "PHP: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "parsa" HTML betyder att behandla HTML-kod för att extrahera och använda specifik information. Det är en vanlig praxis för programmerare när de arbetar med webbplatser eller applikationer, eftersom det gör det möjligt att få tag i specifika data från en webbsida för att sedan manipulera och presentera det på ett lämpligt sätt.

## Hur man gör:
Att parsning av HTML i PHP är ganska enkelt och kan utföras med hjälp av några inbyggda funktioner och metoder i PHP. Här är ett grundläggande exempel på hur man hämtar och skriver ut titlarna på länkar från en webbsida:

```PHP
<?php 
$html = file_get_contents('https://www.sweden.se/'); // hämta HTML-kod från en webbsida
$dom = new DOMDocument(); // skapa en DOMDocument för att få tillgång till HTML-elementen 
$dom->loadHTML($html); // ladda in HTML-koden från webbsidan
$anchors = $dom->getElementsByTagName('a'); // hämta alla <a> element
foreach($anchors as $anchor) { // loopa igenom alla länkar
  echo $anchor->getAttribute('title'); // skriv ut titeln på varje länk
}
?>
```
**Resultat:**
- "In A Class Of Its Own: Discovering English Schools In Sweden"
- "How Sweden's Feminist Foreign Policy Is Changing The World"
- "Nordens Lumsk: Mörkertid Och Monster"
- "So Long Landfill: Zleep Hotels Want To Change The Hotel Industry"
- "Join Us On A Trek Through Sweden's Autumn Wonderland"
- "Midsummer Music Festival Goes Baltic — And Acoustic"

## Djupdykning:
Parsning av HTML-kod har funnits i många år och är en viktig del av webbutvecklingen. Det har också lett till utvecklingen av andra parsningstekniker, såsom regelbundna uttryck, för att lösa specifika problem eller göra parsningen mer effektiv.

När det gäller implementation av parsning av HTML i PHP, finns det flera tredjepartsbibliotek som erbjuder mer avancerade funktioner och metoder för att underlätta parsningsprocessen. Ett exempel är PHP Simple HTML DOM Parser som ger enkla och intuitiva sätt att hämta data från en webbsida.

## Se även:
- [PHP Simple HTML DOM Parser](https://simplehtmldom.sourceforge.io/) - ett tredjepartsbibliotek för parsning av HTML i PHP
- [Official PHP documentation for DOMDocument](https://www.php.net/manual/en/class.domdocument.php) - officiell dokumentation för PHP:s DOMDocument-klass som används för att hantera HTML-dokument
- [Regular Expressions in PHP](https://www.php.net/manual/en/function.preg-match.php) - dokumentation för PHP:s inbyggda funktioner för regelbundna uttryck