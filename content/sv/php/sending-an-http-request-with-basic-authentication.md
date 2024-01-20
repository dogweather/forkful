---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att sända en HTTP-begäran med grundläggande autentisering innebär att inkludera autentiseringsuppgifter som användarnamn och lösenord i vår anrop för att få tillgång till skyddade resurser. Programmerare gör det för att kommunicera skyddat med API:er och webbsidor som kräver inloggning.

## Hur man gör:

Här är ett exempel på hur man skickar en HTTP-begäran med grundläggande autentisering i PHP.

```PHP
<?php
$curl = curl_init();

curl_setopt_array($curl, array(
CURLOPT_URL => 'https://example.com',
CURLOPT_RETURNTRANSFER => true,
CURLOPT_ENCODING => '',
CURLOPT_MAXREDIRS => 10,
CURLOPT_TIMEOUT => 0,
CURLOPT_FOLLOWLOCATION => true,
CURLOPT_HTTP_VERSION => CURL_HTTP_VERSION_1_1,
CURLOPT_CUSTOMREQUEST => 'GET',
CURLOPT_HTTPHEADER => array(
'Authorization: Basic '. base64_encode('username:password')
),
));

$response = curl_exec($curl);

curl_close($curl);
echo $response
?>
```

Detta skript kommer att ge oss svaret från `https://example.com` som en sträng, vilket är autentiserat genom grundläggande autentisering.

## Djupdykning

1. Historisk bakgrund: Den grundläggande autentiseringsmekanismen är en del av HTTP:s tidigare specifikationer och har funnits sedan 1996. Det ger ett sätt att skydda resurser med ett användarnamn och lösenord, krypterade med Base64.

2. Alternativ: Andra autentiseringsmetoder inkluderar tokens (OAuth, JWT etc.) och Digest-access-autentisering. De ger mer säkerhet och funktionalitet jämfört med grundläggande autentisering.

3. Implementeringsdetaljer: I PHP används cURL-funktioner för att skicka HTTP-begäran. `CURLOPT_HTTPHEADER`-alternativet används för att lägga till "Authorization"-headern med base64-kodade legitimationsuppgifter.

## Se även

- PHP cURL bibliotek: https://www.php.net/manual/en/book.curl.php
- HTTP Basic authentication: https://en.wikipedia.org/wiki/Basic_access_authentication