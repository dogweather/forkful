---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & varför?

Att tolka (parse) HTML handlar om att omvandla webbsidors HTML-kod till något mer användbart, ofta en datastruktur. Programmerare gör det vanligtvis för att extrahera viss data från webbsidor.

## Hur gör man:

Använd PHP's inbyggda DOMDocument-klass för att tolka HTML. Här är ett enkelt exempel:

```PHP
<?php
$dom = new DOMDocument;
$dom->loadHTML('<html><body><p>Hej Värld!</p></body></html>');
$p = $dom->getElementsByTagName('p');
echo $p->item(0)->nodeValue;
?>
```
När du kör koden ovan, kommer du att se denna output:

```PHP
Hej Värld!
```
Vår kod har tolkat HTML-strängen och utskrivit texten inom `<p>`-taggen.

## Djupdykning:

Tolka HTML kan vara en komplex uppgift. Tillbaka på 90-talet, var HTML kode ännu inte noggrant standardiserat, vilket gjorde tolkning svår. PHP's DOMDocument, introducerad i PHP 5, följer W3C's Dokument Objek Modell (DOM), som skapades för att ta itu med dessa problem.

Det finns också alternativ till DOMDocument för HTML-tolkning i PHP, som t.ex. Simple HTML DOM Parser och PHP’s inbyggda XML-Parser, men de är inte lika robusta.

När det gäller implementering av HTML-tolkning, representerar DOMDocument HTML-dokumentet som ett trädstruktur av nod-objekt. Varje nod motsvarar en del av dokumentet, som till exempel ett specifikt element eller textbit.

## Se även:

För mer information om att tolka HTML med PHP, rekommenderas följande källor:

- PHP's officiella dokumentation för DOMDocument: [https://www.php.net/manual/en/class.domdocument.php](https://www.php.net/manual/en/class.domdocument.php)
- Simple HTML DOM Parser: [https://simplehtmldom.sourceforge.io/](https://simplehtmldom.sourceforge.io/)