---
title:                "Sända en http-begäran"
html_title:           "PHP: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

Vad & Varför?
När du använder en webbplats, skickar din webbläsare kontinuerligt HTTP-förfrågningar till servern för att få åtkomst till innehållet på sidan. HTTP-förfrågan är vad som gör det möjligt för dig att klicka på länkar, fylla i formulär och ladda ner filer. Det är en viktig del av webbutveckling och används av programmerare för att hämta eller skicka data till en server.

Så här gör du:
För att skicka en HTTP-förfrågan från din PHP-kod, använder du funktionen 'file_get_contents ()'. Denna funktion tar en URL som argument och returnerar innehållet på den sidan som en sträng. Se nedan för ett exempel och den resulterande utmatningen.

``` PHP
$url = "https://www.example.com/";
$response = file_get_contents($url);
echo $response;
```

Utskrift:
``` HTML
<html>
<head>
<title>Exempelsida</title>
</head>
<body>
<h1>Välkommen till exempelsidan</h1>
<p>Detta är en enkel sida som visar vad som är möjligt med HTTP-förfrågningar i PHP.</p>
</body>
</html>
```

Djupdykning:
HTTP-förfrågan är en del av HTTP-protokollet som används för kommunikation mellan webbservrar och webbläsare. Det finns också alternativ till funktionen 'file_get_contents ()' som också kan användas för att skicka HTTP-förfrågningar, som till exempel funktionerna 'curl_init ()' och 'fopen ()'.

När det gäller implementation i PHP, så är 'file_get_contents ()' den enklaste funktionen att använda, men det kan finnas situationer där andra funktioner är mer lämpliga. Det är viktigt att ha en god förståelse för HTTP-protokollet för att effektivt kunna använda dessa funktioner.

Se även:
- Dokumentation för funktionen 'file_get_contents ()' i PHP: https://www.php.net/manual/en/function.file-get-contents
- Mer information om HTTP-protokollet: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview