---
title:                "PHP: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Grundläggande autentisering är en viktig del av webbutveckling och används främst för att skydda åtkomsten till dina webbapplikationer. Genom att skicka en HTTP-begäran med grundläggande autentisering kan du säkra din webbplats och förhindra obehörig åtkomst.

## Hur man gör det

För att skicka en HTTP-begäran med grundläggande autentisering behöver du först skapa en anslutning till den aktuella webbplatsen. Detta kan enkelt göras med PHP-funktionen `curl_init ()` som tar webbadressen som argument.

För att lägga till grundläggande autentisering till din begäran måste du använda `curl_setopt ()` för att ställa in `CURLOPT_HTTPAUTH` till `CURLAUTH_BASIC` och ange de önskade autentiseringsuppgifterna med `CURLOPT_USERPWD`. Se koden nedan för en enkel begäran med grundläggande autentisering:

```PHP
$ch = curl_init("https://example.com");
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "username:password");
$output = curl_exec($ch);
curl_close($ch);

echo $output;
```
Detta kodblock kommer att skapa en anslutning till https://example.com och skicka en begäran med grundläggande autentisering med användarnamnet "användarnamn" och lösenordet "lösenord". Output-variabeln kommer att innehålla svaret från servern, vilket kan inkludera headers, innehåll och statuskod.

## Djupdykning

När en begäran med grundläggande autentisering skickas, inkluderar den en `Authorization` header som innehåller en bas64-kodad sträng med användarnamn och lösenord i formatet "användarnamn:lösenord". För att säkerställa att dina autentiseringsuppgifter är säkra rekommenderas det att du använder HTTPS istället för HTTP.

Det är också viktigt att notera att grundläggande autentisering bara ger grundläggande säkerhet och inte är tillräckligt för att skydda känsliga data. I dessa fall bör du istället använda mer robusta autentiseringssystem som OAuth eller JWT.

## Se även

- [PHP cURL-dokumentationen](https://www.php.net/manual/en/book.curl.php)
- [HTTP Authentication på MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OAuth vs JWT: Vad är skillnaden?](https://auth0.com/blog/oauth-vs-json-web-tokens/)