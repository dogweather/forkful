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

## Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering är ett vanligt sätt att säkra en begäran till ett webb-API. Det krävs oftast när du behöver åtkomst till privata eller skyddade resurser.

## Hur man gör

```PHP
$ch = curl_init(); // Initierar en ny curl-resurs
$url = "https://example.com/api"; // URL:n till API:t
$username = "username"; // Användarnamnet för autentisering
$password = "password"; // Lösenordet för autentisering
$data = array("key" => "value"); // Eventuella parametrar som ska skickas med i förfrågan
$headers = array("Content-Type: application/json"); // Eventuella headers för förfrågan

// Ange autentiseringsinformation i en HTTP-basisk autentiseringsheader
curl_setopt($ch, CURLOPT_HTTPHEADER, array("Authorization: Basic " . base64_encode("$username:$password"))); 

curl_setopt($ch, CURLOPT_URL, $url); // Sätter URL:n för förfrågan
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // Sätter att förfrågan ska returnera data istället för att skriva ut det direkt
curl_setopt($ch, CURLOPT_POST, true); // Sätter att förfrågan ska vara en POST-förfrågan
curl_setopt($ch, CURLOPT_POSTFIELDS, http_build_query($data)); // Omvandlar parametrarna till en sträng för att kunna skickas med i förfrågan
curl_setopt($ch, CURLOPT_HTTPHEADER, $headers); // Sätter eventuella övriga headers för förfrågan

$output = curl_exec($ch); // Utför förfrågan och sparar resultatet i en variabel
curl_close($ch); // Stänger curl-resursen

echo $output; // Skriver ut resultatet
```

**Exempeloutput:**

```
{"success": true, "message": "Data successfully retrieved."}
```

## Djupdykning

HTTP-grundläggande autentisering innebär att användarnamn och lösenord skickas med i varje förfrågan i form av en base64-kodad sträng. Detta skapar en säker kommunikation mellan klienten och servern, men det är fortfarande viktigt att använda HTTPS för att skydda förfrågan från avlyssning.

För att ta emot och verifiera autentiseringsinformationen på servern kan du använda PHP-funktionen `$_SERVER['PHP_AUTH_USER']` och `$_SERVER['PHP_AUTH_PW']`. Det är också vanligt att använda en databas för att lagra användarinformation och jämföra med den som skickas med i förfrågan.

## Se även

- [PHP-cURL funktioner](https://www.php.net/manual/en/ref.curl.php)
- [HTTP-grundläggande autentisering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)