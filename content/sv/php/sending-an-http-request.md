---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran handlar om att be servrar om att få eller ändra data. Programmerare gör detta för att interagera med webbtjänster, API:er och mer.

## Hur man gör:
Låt oss börja med `file_get_contents`. Det är enkelt att förstå och kräver ingen extra installation.
```PHP
<?php
$url = 'http://example.com';
$response = file_get_contents($url);
echo $response;
?>
```
En `file_get_contents`-förfrågan skickar slutresultatet till användaren. Undvik detta för stora filer!

Nu, låt oss gå vidare till `cURL`, ett mer avancerat verktyg tillgängligt i PHP.
```PHP
<?php
$url = 'http://example.com';
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$response = curl_exec($ch);
curl_close ($ch);
echo $response;
?>
```
Med `cURL` kan vi anpassa begäran mycket mer, till exempel skicka POST data eller använda olika HTTP-metoder.

## Djupdykning
Här är lite historisk kontext och alternativ. PHP inkluderade `file_get_contents` i version 4.3.0, och det är överallt idag. Emellertid har `cURL` varit standarden sedemot PHP 4.0.2. Den har mer funktionalitet - men det kan vara lite knepigt för nybörjare eftersom det har fler alternativ.

Vad gäller alternativen, kolla in `fopen`, `fsockopen` och `stream_context_create` för lokala filoperationer. `Guzzle` och `Requests` är också utmärkta bibliotek.

Dessa funktioner bearbetar HTTP-prokollen för dig. När du ber om en webbadress, öppnar det en anslutning till servern, skickar HTTP-begäran och väntar på svaret. Det är precis vad dessa funktioner gör tyst i bakgrunden.

## Se också
- [PHP: HTTP context options - Manual](https://www.php.net/manual/en/context.http.php)
- [PHP: file_get_contents - Manual](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP: cURL - Manual](https://www.php.net/manual/en/book.curl.php)
- [Guzzle, PHP HTTP client](http://guzzlephp.org)
- [Requests for PHP](http://requests.ryanmccue.info)