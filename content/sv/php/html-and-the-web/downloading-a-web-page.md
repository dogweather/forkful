---
date: 2024-01-20 17:44:47.834249-07:00
description: "How to: F\xF6r att ladda ner en webbsida med PHP, anv\xE4nd `file_get_contents()`\
  \ eller cURL-biblioteket. H\xE4r \xE4r grundl\xE4ggande exempel."
lastmod: '2024-03-13T22:44:37.995152-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att ladda ner en webbsida med PHP, anv\xE4nd `file_get_contents()`\
  \ eller cURL-biblioteket."
title: "H\xE4mta en webbsida"
weight: 42
---

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
