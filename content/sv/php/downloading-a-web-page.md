---
title:                "Ladda ner en webbsida"
html_title:           "PHP: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att ladda ner en webbsida innebär att hämta dess innehåll och visa det på din enhet. Programmerare gör detta för att kunna manipulera och analysera innehållet på ett strukturerat sätt.

## Så här:
Ett enkelt sätt att ladda ner en webbsida är att använda funktionen `file_get_contents()` i PHP. Detta tar en URL som parameter och returnerar innehållet som en sträng. Exempel:

```PHP
$webbsida = file_get_contents('https://www.example.com');
echo $webbsida;
```
Output:
```
<!DOCTYPE html>
<html>
<head>
  <title>Exempelwebbplats</title>
</head>
<body>
  <h1>Välkommen</h1>
  <p>Denna webbplats är endast ett exempel.</p>
</body>
</html>
```

## Djupdykning:
Att kunna ladda ner en webbsida är användbart för att till exempel hämta data från en extern källa eller skapa en cache för att förbättra prestandan på en webbplats. Innan `file_get_contents()` infördes, användes funktionen `fopen()` för att öppna en URL som en fil och sedan läsa in innehållet. Detta fungerar fortfarande, men `file_get_contents()` är enklare att använda.

## Se även:
- [PHP's dokumentation för file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Exempel på användning av file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php#example-5446)
- [Alternativ för att ladda ner en webbsida i PHP](https://www.php.net/manual/en/function.file-get-contents.php#example-3009)