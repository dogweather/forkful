---
title:                "Hämta en webbsida"
aliases:
- /sv/php/downloading-a-web-page/
date:                  2024-01-20T17:44:47.834249-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Att ladda ner en webbsida är att hämta allt dess innehåll för att använda eller bearbeta lokalt. Programmerare gör det för att samla data, testa online-tjänsters tillgänglighet, eller skapa säkerhetskopior av webbsidor.

## How to:
För att ladda ner en webbsida med PHP, använd `file_get_contents()` eller cURL-biblioteket. Här är grundläggande exempel:

```php
<?php
// Använda file_get_contents()
$htmlContent = file_get_contents('http://example.com');
echo $htmlContent;

// Använda cURL
$ch = curl_init('http://example.com');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$htmlContent = curl_exec($ch);
curl_close($ch);
echo $htmlContent;
?>
```

Förväntad utdata är HTML-innehållet från 'http://example.com'.

## Deep Dive
Innan `file_get_contents()` och cURL dök upp, användes GP/IP socket-anslutningar för att ladda ner webbsidor – klumpigare och mer lågnivå. `file_get_contents()` är enkel men erbjuder mindre kontroll, medan cURL stödjer omfattande alternativ som headers, HTTP-metoder och cookies, vilket är viktigt när du hanterar mer komplexa scenarion.

Förutom dessa två finns bibliotek som Guzzle för ännu mer avancerad hantering av HTTP-förfrågningar i PHP. Guzzle ger en modern, kraftfull och flexibel HTTP klient för att bygga och skicka förfrågningar.

När det gäller implementationen bör vi överväga tid för timeout, felsökning och hantering av omdirigeringar. Kom ihåg att konfigurera PHP för att tillåta externa URL-förfrågningar när du använder `file_get_contents()`.

## See Also
- PHP cURL dokumentation: https://www.php.net/manual/en/book.curl.php
- PHP `file_get_contents()` dokumentation: https://www.php.net/manual/en/function.file-get-contents.php
- Guzzle HTTP klient: http://docs.guzzlephp.org/en/stable/
