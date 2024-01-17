---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "PHP: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering är en vanlig teknik som används av programmerare för att verifiera användare och utföra åtgärder på webbplatser och webbtjänster. Detta gör det möjligt för webbplatser att begränsa åtkomsten till vissa resurser och skydda känslig information från obehöriga användare.

## Hur man gör:
För att skicka en HTTP-förfrågan med grundläggande autentisering i PHP, måste du använda funktionen `curl_init()` för att initiera en ny cURL-session. Sedan lägger du till nödvändig information som användarnamn och lösenord i en HTTP-autentiseringsheader med hjälp av funktionen `curl_setopt()`. Slutligen utför du förfrågan genom att använda funktionen `curl_exec()` och få svar från webbtjänsten.

```PHP
$ch = curl_init();
$url = 'http://example.com/api/';
$username = 'användarnamn';
$password = 'lösenord';

curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$response = curl_exec($ch);
curl_close($ch);

echo $response; //visar svar från webbtjänsten
```

## Utforska vidare:
Att skicka en HTTP-förfrågan med grundläggande autentisering använder sig av en HTTP-autentiseringsheader som innehåller en Base64-kodad version av användarnamn och lösenord. Den här metoden är enkel men inte särskilt säker då både användarnamn och lösenord skickas i klartext. Det finns alternativ som OAuth som ger en mer säker autentiseringsprocess. För att implementera detta i din kod, kan du använda OAuth-bibliotek för PHP.

## Se även:
- [PHP manualen om cURL](https://www.php.net/manual/en/book.curl.php)
- [OAuth-bibliotek för PHP](https://oauth.net/code/php/)